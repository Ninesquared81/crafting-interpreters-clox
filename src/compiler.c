#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "chunk.h"
#include "loop.h"
#include "memory.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    Token current;
    Token previous;
    bool had_error;
    bool panic_mode;
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,    // =
    PREC_CONDITIONAL,   // ?:
    PREC_OR,            // or
    PREC_AND,           // and
    PREC_EQUALITY,      // == !=
    PREC_COMPARISON,    // < > <= >=
    PREC_TERM,          // + -
    PREC_FACTOR,        // * /
    PREC_UNARY,         // ! -
    PREC_CALL,          // . ()
    PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool can_assign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct {
    Token name;
    int depth;
    bool is_mutable;
} Local;

typedef enum {
    TYPE_FUNCTION,
    TYPE_SCRIPT,
} FunctionType;

typedef struct Compiler {
    struct Compiler *enclosing;
    ObjFunction *function;
    FunctionType type;

    Local *locals;
    uint32_t local_count;
    uint32_t local_capacity;
    int scope_depth;
    LoopStack loops;
} Compiler;

Parser parser;
Compiler *current = NULL;

static Chunk *current_chunk(void) {
    return &current->function->chunk;
}

static void error_at(Token *token, const char *message) {
    if (parser.panic_mode) return;
    parser.panic_mode = true;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    }
    else if (token->type == TOKEN_ERROR) {

    }
    else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.had_error = true;
}

static void error(const char *message) {
    error_at(&parser.previous, message);
}

static void error_at_current(const char *message) {
    error_at(&parser.current, message);
}

static void advance(void) {
    parser.previous = parser.current;
    for (;;) {
        parser.current = scan_token();
        if (parser.current.type != TOKEN_ERROR) break;
        
        error_at_current(parser.current.start);
    }
}

static void consume(TokenType type, const char *message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    error_at_current(message);
}

static bool check(TokenType type) {
    return parser.current.type == type;
}

static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

static void emit_byte(uint8_t byte) {
    write_chunk(current_chunk(), byte, parser.previous.line);
}

static void emit_bytes(uint8_t byte1, uint8_t byte2) {
    emit_byte(byte1);
    emit_byte(byte2);
}

static void emit_loop(int loop_start) {
    emit_byte(OP_LOOP);

    int offset = current_chunk()->count - loop_start + 2;
    if (offset > UINT16_MAX) error("Loop body too large.");

    emit_byte((offset >> 8) & 0xff);
    emit_byte(offset & 0xff);
}

static int emit_jump(uint8_t instruction) {
    emit_byte(instruction);
    emit_byte(0xff);
    emit_byte(0xff);
    return current_chunk()->count - 2;
}

static void emit_return(void) {
    emit_byte(OP_NIL);
    emit_byte(OP_RETURN);
}

static uint32_t make_constant(Value value) {
    uint32_t constant = add_constant(current_chunk(), value);
    // Check the higher bits are not set.
    if (constant & 0xff000000) {
        error("Too many constants in one chunk");
        return 0;
    }
    
    return constant;
}

static void emit_varint_instruction(uint8_t instruction, uint32_t operand) {
    // Emit opcode.
    emit_byte(instruction);
    if (IS_LONG_INSTRUCTION(instruction)) {
        // Emit two leading bytes of index.
        emit_bytes(operand >> 16, operand >> 8);
    }
    // Emit final byte.
    emit_byte((uint8_t)operand);
}
    

static void emit_constant(Value value) {
    uint32_t constant = make_constant(value);
    uint8_t instruction = (constant <= UINT8_MAX) ? OP_CONSTANT : OP_CONSTANT_LONG;

    emit_varint_instruction(instruction, constant);
}

static void emit_popn(uint32_t pop_count) {
    if (pop_count == 0) {
        return;
    }
    if (pop_count == 1) {
        emit_byte(OP_POP);
        return;
    }
    
    uint8_t instruction = (pop_count <= UINT8_MAX) ? OP_POPN : OP_POPN_LONG;
    emit_varint_instruction(instruction, pop_count);
}    


static void patch_jump(int offset) {
    // -2 to adjust for the bytecode for the jump offset itself.
    int jump = current_chunk()->count - offset - 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over.");
    }

    current_chunk()->code[offset] = (jump >> 8) & 0xff;
    current_chunk()->code[offset + 1] = jump & 0xff;
}

static void patch_loop(int offset, int loop_start) {
    // Need to add 2 since we're jumping BACKWARDS.
    int jump = offset - loop_start + 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over.");
    }

    current_chunk()->code[offset] = (jump >> 8) & 0xff;
    current_chunk()->code[offset + 1] = jump & 0xff;
}

static void finish_loop(Loop loop, int loop_start) {
    // Loop through each break statement inside loop and patch them.
    for (int *jump = loop.breaks.offsets; jump < loop.breaks.offsets + loop.breaks.count; ++jump) {
        patch_jump(*jump);
    }
    // Loop through each continue statement inside loop and patch them.
    for (int *jump = loop.continues.offsets; jump < loop.continues.offsets + loop.continues.count; ++jump) {
        patch_loop(*jump, loop_start);
    }
}

static void pop_locals(int target_depth) {
    uint32_t pop_count = 0;
    for (int i = current->local_count; i > 0 && current->locals[i - 1].depth > target_depth; --i) {
        ++pop_count;
    }
    emit_popn(pop_count);
}

static void init_compiler(Compiler *compiler, FunctionType type) {
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = type;
    compiler->locals = NULL;
    compiler->local_count = 0;
    compiler->local_capacity = GROW_CAPACITY(0);
    compiler->locals = ALLOCATE(Local, compiler->local_capacity);
    compiler->scope_depth = 0;
    compiler->function = new_function();
    init_loop_stack(&compiler->loops);
    current = compiler;
    if (type != TYPE_SCRIPT) {
        current->function->name = copy_string(parser.previous.start, parser.previous.length);
    }
    
    Local *local = &current->locals[current->local_count++];
    local->depth = 0;
    local->name.start = "";
    local->name.length = 0;
}

static void free_compiler(Compiler *compiler) {
    FREE_ARRAY(Local, compiler->locals, compiler->local_count);
    compiler->locals = NULL;
    compiler->local_count = 0;
    compiler->local_capacity = 0;
    compiler->scope_depth = 0;
    free_loop_stack(&compiler->loops);
}

static ObjFunction *end_compiler(void) {
    emit_return();
    ObjFunction *function = current->function;

    free_compiler(current);
    
#ifdef DEBUG_PRINT_CODE
    if (!parser.had_error) {
        disassemble_chunk(current_chunk(), (function->name != NULL)
                          ? function->name->chars : "<script>");
    }
#endif

    current = current->enclosing;
    return function;
}

static void begin_scope(void) {
    current->scope_depth++;
}

static void end_scope(void) {
    current->scope_depth--;

    uint32_t pop_count = 0;
    while (current->local_count > 0 &&
           current->locals[current->local_count - 1].depth > current->scope_depth) {
        pop_count++;
        current->local_count--;
    }

    emit_popn(pop_count);
}

static void expression(void);
static void statement(void);
static void declaration(void);
static ParseRule *get_rule(TokenType token);
static void parse_precedence(Precedence precedence);

static uint32_t identifier_constant(Token *name) {
    return make_constant(OBJ_VAL(copy_string(name->start, name->length)));
}

static bool identifiers_equal(Token *a, Token *b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static uint32_t resolve_local(Compiler *compiler, Token *name) {
    for (Local *iter = compiler->locals + compiler->local_count; iter > compiler->locals; --iter) {
        Local *local = iter - 1;
        if (identifiers_equal(name, &local->name)) {
            if (local->depth == -1) {
                error("Can't read local variable in its own initializer.");
            }
            return local - compiler->locals;
        }
    }

    return -1;
}

static void add_local(Token name, bool is_mutable) {
    if (current->local_count == UINT24_COUNT) {
        error("Too many local variables in function.");
        return;
    }
    if (current->local_capacity < current->local_count + 1) {
        uint32_t old_capacity = current->local_capacity;
        current->local_capacity = GROW_CAPACITY(old_capacity);
        current->locals = GROW_ARRAY(Local, current->locals, old_capacity, current->local_capacity);
    }
    
    Local *local = &current->locals[current->local_count++];
    local->name = name;
    local->depth = -1;
    local->is_mutable = is_mutable;
}

static void declare_variable(bool is_mutable) {
    if (current->scope_depth == 0) return;

    Token *name = &parser.previous;
    for (Local *iter = current->locals + current->local_count; iter > current->locals; --iter) {
        Local *local = iter - 1;
        if (local->depth != -1 && local->depth < current->scope_depth) {
            break;
        }

        if (identifiers_equal(name, &local->name)) {
            error("Already a variable with this name in this scope.");
        }
    }
    
    add_local(*name, is_mutable);
}

static uint32_t parse_variable(const char *error_message, bool is_mutable) {
    consume(TOKEN_IDENTIFIER, error_message);

    declare_variable(is_mutable);
    if (current->scope_depth > 0) return 0;
    
    return identifier_constant(&parser.previous);
}

static void mark_initialized(void) {
    if (current->scope_depth == 0) return;
    current->locals[current->local_count - 1].depth = current->scope_depth;
}

static void define_variable(uint32_t global, bool is_mutable) {
    if (current->scope_depth > 0) {
        mark_initialized();
        return;
    }
    
    uint8_t instruction = (global <= UINT8_MAX) ? OP_DEFINE_GLOBAL : OP_DEFINE_GLOBAL_LONG;
    emit_varint_instruction(instruction, global);
    emit_byte(is_mutable);
}

static uint32_t argument_list(void) {
    uint32_t arg_count = 0;
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            expression();
            if (arg_count == UINT24_MAX) {
                error("Too many arguments in function call");
            }
            arg_count++;
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return arg_count;
}

static void and(bool can_assign) {
    (void)can_assign;

    int end_jump = emit_jump(OP_JUMP_IF_FALSE);

    emit_byte(OP_POP);
    parse_precedence(PREC_AND);

    patch_jump(end_jump);
}

static void binary(bool can_assign) {
    (void)can_assign;
    TokenType operator_type = parser.previous.type;
    ParseRule *rule = get_rule(operator_type);
    parse_precedence((Precedence)(rule->precedence + 1));
    switch (operator_type) {
    case TOKEN_BANG_EQUAL:    emit_bytes(OP_EQUAL, OP_NOT); break;
    case TOKEN_EQUAL_EQUAL:   emit_byte(OP_EQUAL); break;
    case TOKEN_GREATER:       emit_byte(OP_GREATER); break;
    case TOKEN_GREATER_EQUAL: emit_bytes(OP_LESS, OP_NOT); break;
    case TOKEN_LESS:          emit_byte(OP_LESS); break;
    case TOKEN_LESS_EQUAL:    emit_bytes(OP_GREATER, OP_NOT); break;
    case TOKEN_PLUS:          emit_byte(OP_ADD); break;
    case TOKEN_MINUS:         emit_byte(OP_SUBTRACT); break;
    case TOKEN_STAR:          emit_byte(OP_MULTIPLY); break;
    case TOKEN_SLASH:         emit_byte(OP_DIVIDE); break;
    default: return;  // Unreachable.
    }
}

static void call(bool can_assign) {
    (void)can_assign;
    uint32_t arg_count = argument_list();
    uint8_t instruction = (arg_count <= UINT8_MAX) ? OP_CALL : OP_CALL_LONG;
    emit_varint_instruction(instruction, arg_count);
}

static void conditional(bool can_assign) {
    (void)can_assign;
    // condition is at top of stack
    int then_jump = emit_jump(OP_JUMP_IF_FALSE);
    emit_byte(OP_POP);
    expression();  // true-case value (i.e. between '?' and ':').
    int else_jump = emit_jump(OP_JUMP);
    patch_jump(then_jump);

    emit_byte(OP_POP);
    consume(TOKEN_COLON, "Expect ':' after expression.");
    parse_precedence(PREC_CONDITIONAL);  // false-case value (i.e. after ':').
    patch_jump(else_jump);
}

static void literal(bool can_assign) {
    (void)can_assign;
    switch (parser.previous.type) {
    case TOKEN_FALSE: emit_byte(OP_FALSE); break;
    case TOKEN_NIL: emit_byte(OP_NIL); break;
    case TOKEN_TRUE: emit_byte(OP_TRUE); break;
    default: return;  // Unreachable.
    }
}

static void grouping(bool can_assign) {
    (void)can_assign;
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(bool can_assign) {
    (void)can_assign;
    double value = strtod(parser.previous.start, NULL);
    emit_constant(NUMBER_VAL(value));
}

static void or(bool can_assign) {
    (void)can_assign;

    int else_jump = emit_jump(OP_JUMP_IF_FALSE);
    int end_jump = emit_jump(OP_JUMP);

    patch_jump(else_jump);
    emit_byte(OP_POP);

    parse_precedence(PREC_OR);
    patch_jump(end_jump);
}

static void string(bool can_assign) {
    (void)can_assign;
    emit_constant(OBJ_VAL(copy_string(parser.previous.start + 1,
                                      parser.previous.length - 2)));
}

static void named_variable(Token name, bool can_assign) {
    uint32_t arg = resolve_local(current, &name);
    uint8_t get_op, set_op;
    bool is_local = false;

    if (arg != (unsigned)-1) {
        is_local = true;
        get_op = (arg <= UINT8_MAX) ? OP_GET_LOCAL : OP_GET_LOCAL_LONG;
        set_op = (arg <= UINT8_MAX) ? OP_SET_LOCAL : OP_SET_LOCAL_LONG;
    }
    else {
        arg = identifier_constant(&name);
        get_op = (arg <= UINT8_MAX) ? OP_GET_GLOBAL : OP_GET_GLOBAL_LONG;
        set_op = (arg <= UINT8_MAX) ? OP_SET_GLOBAL : OP_SET_GLOBAL_LONG;
    }

    if (can_assign && match(TOKEN_EQUAL)) {
        if (is_local && !current->locals[arg].is_mutable) {
            error("Cannot assign to a val variable.");
        }
        expression();
        emit_varint_instruction(set_op, arg);
    }
    else {
        emit_varint_instruction(get_op, arg);
    }
}

static void variable(bool can_assign) {
    named_variable(parser.previous, can_assign);
}

static void unary(bool can_assign) {
    (void)can_assign;
    TokenType operator_type = parser.previous.type;

    // Compile the operand.
    parse_precedence(PREC_UNARY);

    // Emit the operator instruction.
    switch (operator_type) {
    case TOKEN_BANG: emit_byte(OP_NOT); break;
    case TOKEN_MINUS: emit_byte(OP_NEGATE); break;
    default: return;  // Unreachable.
    }
}

ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]    = {grouping,   call,        PREC_CALL},
    [TOKEN_RIGHT_PAREN]   = {NULL,       NULL,        PREC_NONE},
    [TOKEN_LEFT_BRACE]    = {NULL,       NULL,        PREC_NONE},
    [TOKEN_RIGHT_BRACE]   = {NULL,       NULL,        PREC_NONE},
    [TOKEN_COLON]         = {NULL,       NULL,        PREC_NONE},
    [TOKEN_COMMA]         = {NULL,       NULL,        PREC_NONE},
    [TOKEN_DOT]           = {NULL,       NULL,        PREC_NONE},
    [TOKEN_MINUS]         = {unary,      binary,      PREC_TERM},
    [TOKEN_PLUS]          = {NULL,       binary,      PREC_TERM},
    [TOKEN_QUESTION_MARK] = {NULL,       conditional, PREC_CONDITIONAL},
    [TOKEN_SEMICOLON]     = {NULL,       NULL,        PREC_NONE},
    [TOKEN_SLASH]         = {NULL,       binary,      PREC_FACTOR},
    [TOKEN_STAR]          = {NULL,       binary,      PREC_FACTOR},
    [TOKEN_BANG]          = {unary,      NULL,        PREC_NONE},
    [TOKEN_BANG_EQUAL]    = {NULL,       binary,      PREC_EQUALITY},
    [TOKEN_EQUAL]         = {NULL,       NULL,        PREC_NONE},
    [TOKEN_EQUAL_EQUAL]   = {NULL,       binary,      PREC_EQUALITY},
    [TOKEN_GREATER]       = {NULL,       binary,      PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL,       binary,      PREC_COMPARISON},
    [TOKEN_LESS]          = {NULL,       binary,      PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]    = {NULL,       binary,      PREC_COMPARISON},
    [TOKEN_IDENTIFIER]    = {variable,   NULL,        PREC_NONE},
    [TOKEN_STRING]        = {string,     NULL,        PREC_NONE},
    [TOKEN_NUMBER]        = {number,     NULL,        PREC_NONE},
    [TOKEN_AND]           = {NULL,       and,         PREC_AND},
    [TOKEN_CLASS]         = {NULL,       NULL,        PREC_NONE},
    [TOKEN_ELSE]          = {NULL,       NULL,        PREC_NONE},
    [TOKEN_FALSE]         = {literal,    NULL,        PREC_NONE},
    [TOKEN_FOR]           = {NULL,       NULL,        PREC_NONE},
    [TOKEN_FUN]           = {NULL,       NULL,        PREC_NONE},
    [TOKEN_IF]            = {NULL,       NULL,        PREC_NONE},
    [TOKEN_INPUT]         = {NULL,       NULL,        PREC_NONE},
    [TOKEN_NIL]           = {literal,    NULL,        PREC_NONE},
    [TOKEN_OR]            = {NULL,       or,          PREC_OR},
    [TOKEN_PRINT]         = {NULL,       NULL,        PREC_NONE},
    [TOKEN_RETURN]        = {NULL,       NULL,        PREC_NONE},
    [TOKEN_SUPER]         = {NULL,       NULL,        PREC_NONE},
    [TOKEN_THIS]          = {NULL,       NULL,        PREC_NONE},
    [TOKEN_TRUE]          = {literal,    NULL,        PREC_NONE},
    [TOKEN_VAL]           = {NULL,       NULL,        PREC_NONE},
    [TOKEN_VAR]           = {NULL,       NULL,        PREC_NONE},
    [TOKEN_WHILE]         = {NULL,       NULL,        PREC_NONE},
    [TOKEN_ERROR]         = {NULL,       NULL,        PREC_NONE},
    [TOKEN_EOF]           = {NULL,       NULL,        PREC_NONE},
};
    
static void parse_precedence(Precedence precedence) {
    advance();
    ParseFn prefix_rule = get_rule(parser.previous.type)->prefix;
    if (prefix_rule == NULL) {
        error("Expect expression.");
        return;
    }

    bool can_assign = precedence <= PREC_ASSIGNMENT;
    prefix_rule(can_assign);

    while (precedence <= get_rule(parser.current.type)->precedence) {
        advance();
        ParseFn infix_rule = get_rule(parser.previous.type)->infix;
        infix_rule(can_assign);
    }

    if (can_assign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }
}

static ParseRule *get_rule(TokenType type) {
    return &rules[type];
}
    
static void expression(void) {
    parse_precedence(PREC_ASSIGNMENT);
}

static void block(void) {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type) {
    Compiler compiler;
    init_compiler(&compiler, type);
    begin_scope();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
    if (!check(TOKEN_RIGHT_PAREN)) {
        do {
            current->function->arity++;
            if (current->function->arity > 255) {
                error_at_current("Can't have more than 255 parameters.");
            }
            uint32_t constant = parse_variable("Expect parameter name.", true);
            define_variable(constant, true);
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    block();

    ObjFunction *function = end_compiler();
    emit_bytes(OP_CONSTANT, make_constant(OBJ_VAL(function)));
}

static void fun_declaration(void) {
    uint32_t global = parse_variable("Expect function name.", false);
    mark_initialized();
    function(TYPE_FUNCTION);
    define_variable(global, false);
}

static void var_declaration(bool is_mutable) {
    uint32_t global = parse_variable("Expect variable name.", is_mutable);

    if (match(TOKEN_EQUAL)) {
        expression();
    }
    else {
        if (is_mutable) {
            emit_byte(OP_NIL);
        }
        else {
            error("Val declaration must have an initializer.");
            return;
        }
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    define_variable(global, is_mutable);
}

static void break_statement(void) {
    if (IS_LOOP_STACK_EMPTY(current->loops)) {
        error("Cannot have 'break' outside of loop.");
        return;
    }
    
    // End all nested scoped before jumping.
    pop_locals(peek_scope_depth(&current->loops));

    int jump = emit_jump(OP_JUMP);
    // Jump is patched by containing loop.

    push_break(&current->loops, jump);

    consume(TOKEN_SEMICOLON, "Expect ';' after 'break'.");
}

static void continue_statement(void) {
    if (IS_LOOP_STACK_EMPTY(current->loops)) {
        error("Cannot have 'continue' outside of loop.");
        return;
    }

    // End all nested scopes before jumping.
    pop_locals(peek_scope_depth(&current->loops));

    int jump = emit_jump(OP_LOOP);  // Needs to be patched by containing loop.

    push_continue(&current->loops, jump);

    consume(TOKEN_SEMICOLON, "Expect ';' after 'continue'.");
}

static void expression_statement(void) {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emit_byte(OP_POP);
}

static void for_statement(void) {
    begin_scope();
    push_loop_stack(&current->loops, NEW_LOOP(current->scope_depth));

    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    if (match(TOKEN_SEMICOLON)) {
        // No initializer.
    }
    else if (match(TOKEN_VAR)) {
        var_declaration(true);
    }
    else if (match(TOKEN_VAL)) {
        var_declaration(false);
    }
    else {
        expression_statement();
    }

    int loop_start = current_chunk()->count;
    int exit_jump = -1;
    if (!match(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        // Jump out of the loop if the condition is false.
        exit_jump = emit_jump(OP_JUMP_IF_FALSE);
        emit_byte(OP_POP);  // Condition
    }

    if (!match(TOKEN_RIGHT_PAREN)) {
        int body_jump = emit_jump(OP_JUMP);
        int increment_start = current_chunk()->count;
        expression();
        emit_byte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        emit_loop(loop_start);
        loop_start = increment_start;
        patch_jump(body_jump);
    }

    statement();
    emit_loop(loop_start);

    if (exit_jump != -1) {
        patch_jump(exit_jump);
        emit_byte(OP_POP);
    }

    Loop loop = pop_loop_stack(&current->loops);
    finish_loop(loop, loop_start);

    end_scope();
}

static void if_statement(void) {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int then_jump = emit_jump(OP_JUMP_IF_FALSE);
    emit_byte(OP_POP);
    statement();

    int else_jump = emit_jump(OP_JUMP);
    
    patch_jump(then_jump);
    emit_byte(OP_POP);

    if (match(TOKEN_ELSE)) statement();
    patch_jump(else_jump);
}

static void input_statement(void) {
    emit_byte(OP_INPUT);  // Pushes input to the stack.
    consume(TOKEN_IDENTIFIER, "Expect variable name after 'input'.");
    Token *name = &parser.previous;
    uint32_t arg = resolve_local(current, name);
    uint8_t set_op, set_long_op;
    if (arg != (unsigned)-1) {
        set_op = OP_SET_LOCAL;
        set_long_op = OP_SET_LOCAL_LONG;
    }
    else {
        arg = identifier_constant(name);
        set_op = OP_SET_GLOBAL;
        set_long_op = OP_SET_GLOBAL_LONG;
    }
    uint8_t instruction = (arg <= UINT8_MAX) ? set_op : set_long_op;
    emit_varint_instruction(instruction, arg);
    
    consume(TOKEN_SEMICOLON, "Expect ';' after variable name.");
}

static void print_statement(void) {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emit_byte(OP_PRINT);
}

static void return_statement(void) {
    if (current->type == TYPE_SCRIPT) {
        error("Can't return from top-level code.");
    }

    if (match(TOKEN_SEMICOLON)) {
        emit_return();
    }
    else {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
        emit_byte(OP_RETURN);
    }
}

static void while_statement(void) {
    push_loop_stack(&current->loops, NEW_LOOP(current->scope_depth));
    
    int loop_start = current_chunk()->count;
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int exit_jump = emit_jump(OP_JUMP_IF_FALSE);
    emit_byte(OP_POP);
    statement();
    emit_loop(loop_start);
    
    patch_jump(exit_jump);
    emit_byte(OP_POP);
    
    Loop loop = pop_loop_stack(&current->loops);
    finish_loop(loop, loop_start);
}

static void synchronize(void) {
    parser.panic_mode = false;

    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;
        switch (parser.current.type) {
        case TOKEN_CLASS:
        case TOKEN_FUN:
        case TOKEN_VAL:
        case TOKEN_VAR:
        case TOKEN_FOR:
        case TOKEN_IF:
        case TOKEN_WHILE:
        case TOKEN_PRINT:
        case TOKEN_RETURN:
            return;

        default:
            ;  // Do nothing.
        }

        advance();
    }
}

static void declaration(void) {
    if (match(TOKEN_FUN)) {
        fun_declaration();
    }
    else if (match(TOKEN_VAR)) {
        var_declaration(true);
    }
    else if (match(TOKEN_VAL)) {
        var_declaration(false);
    }
    else {
        statement();
    }

    if (parser.panic_mode) synchronize();
}

static void statement(void) {
    if (match(TOKEN_PRINT)) {
        print_statement();
    }
    else if (match(TOKEN_INPUT)) {
        input_statement();
    }
    else if (match(TOKEN_FOR)) {
        for_statement();
    }
    else if (match(TOKEN_IF)) {
        if_statement();
    }
    else if (match(TOKEN_RETURN)) {
        return_statement();
    }
    else if (match(TOKEN_WHILE)) {
        while_statement();
    }
    else if (match(TOKEN_LEFT_BRACE)) {
        begin_scope();
        block();
        end_scope();
    }
    else if (match(TOKEN_BREAK)) {
        break_statement();
    }
    else if (match(TOKEN_CONTINUE)) {
        continue_statement();
    }
    else {
        expression_statement();
    }
}

ObjFunction *compile(const char *source) {
    init_scanner(source);
    Compiler compiler;
    init_compiler(&compiler, TYPE_SCRIPT);

    parser.had_error = false;
    parser.panic_mode = false;

    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }
    
    ObjFunction *function = end_compiler();
    return (parser.had_error) ? NULL : function;
}
