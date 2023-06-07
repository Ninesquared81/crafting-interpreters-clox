#include <assert.h>
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
    bool is_captured;
    bool is_alive;
} Local;

typedef struct {
    uint32_t index;
    bool is_local;
} Upvalue;

typedef enum {
    TYPE_FUNCTION,
    TYPE_INITIALIZER,
    TYPE_METHOD,
    TYPE_SCRIPT,
} FunctionType;

typedef struct Compiler {
    struct Compiler *enclosing;
    ObjFunction *function;
    FunctionType type;

    Local *locals;
    ulong local_count;
    ulong local_capacity;
    Upvalue *upvalues;
    ulong upvalue_capacity;
    int scope_depth;
    LoopStack loops;
} Compiler;

typedef struct ClassCompiler {
    struct ClassCompiler *enclosing;
} ClassCompiler;

Parser parser;
Compiler *current = NULL;
ClassCompiler *current_class = NULL;

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
    if (current->type == TYPE_INITIALIZER) {
        emit_bytes(OP_GET_LOCAL, 0);
    }
    else {
        emit_byte(OP_NIL);
    }

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

static void emit_varint_instruction(OpCode instruction, uint32_t operand) {
    // Implementation Note:
    // This function relies on the fact that OP_x + 1 == OP_x_LONG.
    assert(IS_LONG_INSTRUCTION(instruction + 1));

    if (operand > UINT8_MAX) {
        instruction++;  // Long version of instruction.
    }

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
    emit_varint_instruction(OP_CONSTANT, constant);
}

static void emit_popn(uint32_t pop_count) {
    if (pop_count == 0) {
        return;
    }
    if (pop_count == 1) {
        emit_byte(OP_POP);
        return;
    }
    
    emit_varint_instruction(OP_POPN, pop_count);
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
    compiler->upvalues = NULL;
    compiler->upvalue_capacity = 0;
    compiler->scope_depth = 0;
    compiler->function = new_function();
    init_loop_stack(&compiler->loops);
    current = compiler;
    if (type != TYPE_SCRIPT) {
        current->function->name = copy_string(parser.previous.start, parser.previous.length);
    }
    
    Local *local = &current->locals[current->local_count++];
    local->depth = 0;
    local->is_mutable = false;
    local->is_captured = false;
    local->is_alive = true;
    if (type != TYPE_FUNCTION) {
        local->name.start = "this";
        local->name.length = 4;
    }
    else {
        local->name.start = "";
        local->name.length = 0;
    }
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
        if (current->locals[current->local_count - 1].is_captured) {
            emit_popn(pop_count);  // Note: emit_popn() correctly handles the case for 0 pops.
            pop_count = 0;
            emit_byte(OP_CLOSE_UPVALUE);
        }
        else {
            pop_count++;
        }
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
            if (!local->is_alive) {
                // Local version of variable has been deleted; assume non-local.
                return -1;
            }
            return local - compiler->locals;
        }
    }

    return -1;
}

static uint32_t add_upvalue(Compiler *compiler, uint32_t index, bool is_local) {
    ulong upvalue_count = compiler->function->upvalue_count;

    for (ulong i = 0; i < upvalue_count; ++i) {
        Upvalue *upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->is_local == is_local) {
            return i;
        }
    }

    if (upvalue_count == UINT24_COUNT) {
        error("Too many closure variables in function.");
        return 0;
    }

    if (compiler->upvalue_capacity < upvalue_count + 1) {
        ulong old_capacity = compiler->upvalue_capacity;
        compiler->upvalue_capacity = GROW_CAPACITY(old_capacity);
        compiler->upvalues = GROW_ARRAY(Upvalue, compiler->upvalues, old_capacity, compiler->upvalue_capacity);
    }
    
    compiler->upvalues[upvalue_count].is_local = is_local;
    compiler->upvalues[upvalue_count].index = index;
    return compiler->function->upvalue_count++;
}

static uint32_t resolve_upvalue(Compiler *compiler, Token *name) {
    if (compiler->enclosing == NULL) return -1;

    uint32_t local = resolve_local(compiler->enclosing, name);
    if (local != (unsigned)-1) {
        compiler->enclosing->locals[local].is_captured = true;
        return add_upvalue(compiler, local, true);
    }

    uint32_t upvalue = resolve_upvalue(compiler->enclosing, name);
    if (upvalue != (unsigned)-1) {
        return add_upvalue(compiler, upvalue, false);
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
    local->is_captured = false;
    local->is_alive = true;
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
    
    emit_varint_instruction(OP_DEFINE_GLOBAL, global);
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
    emit_varint_instruction(OP_CALL, arg_count);
}

static void dot(bool can_assign) {
    consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
    uint32_t name = identifier_constant(&parser.previous);

    if (can_assign && match(TOKEN_EQUAL)) {
        expression();
        emit_varint_instruction(OP_SET_PROPERTY, name);
    }
    else {
        emit_varint_instruction(OP_GET_PROPERTY, name);
    }
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
        get_op = OP_GET_LOCAL;
        set_op = OP_SET_LOCAL;
    }
    else if ((arg = resolve_upvalue(current, &name)) != (unsigned)-1) {
        get_op = OP_GET_UPVALUE;
        set_op = OP_SET_UPVALUE;
    }
    else {
        arg = identifier_constant(&name);
        get_op = OP_GET_GLOBAL;
        set_op = OP_SET_GLOBAL;
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

static void this(bool can_assign) {
    (void)can_assign;
    if (current_class == NULL) {
        error("Can't use 'this' outside of a class.");
        return;
    }

    variable(false);
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
    [TOKEN_DOT]           = {NULL,       dot,         PREC_CALL},
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
    [TOKEN_DEL]           = {NULL,       NULL,        PREC_NONE},
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
    [TOKEN_THIS]          = {this,       NULL,        PREC_NONE},
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
    uint32_t function_constant = make_constant(OBJ_VAL(function));
    if (function->upvalue_count == 0) {
        // No need for a closure.
        emit_varint_instruction(OP_CONSTANT, function_constant);
        return;
    }
    emit_varint_instruction(OP_CLOSURE, function_constant);

    for (ulong i = 0; i < function->upvalue_count; ++i) {
        uint32_t index = compiler.upvalues[i].index;
        bool is_long = (index > UINT8_MAX);
        uint8_t first_byte = (compiler.upvalues[i].is_local) ^ (is_long << 7);  // byte stores (is_long) 0 0 0 0 0 0 (is_local)
        uint8_t last_byte = index;
        emit_byte(first_byte);
        if (is_long) {
            emit_bytes(index >> 16, index >> 8);
        }
        emit_byte(last_byte);
    }
}

static void method(void) {
    consume(TOKEN_IDENTIFIER, "Expect mathod name.");
    uint32_t constant = identifier_constant(&parser.previous);

    FunctionType type = TYPE_METHOD;
    if (parser.previous.length == 4 && memcmp(parser.previous.start, "init", 4) == 0) {
        type = TYPE_INITIALIZER;
    }

    function(type);
    emit_varint_instruction(OP_METHOD, constant);
}

static void class_declaration(void) {
    consume(TOKEN_IDENTIFIER, "Expect a class name.");
    Token class_name = parser.previous;
    uint32_t name_constant = identifier_constant(&parser.previous);
    declare_variable(false);

    emit_varint_instruction(OP_CLASS, name_constant);
    define_variable(name_constant, false);

    ClassCompiler class_compiler;
    class_compiler.enclosing = current_class;
    current_class = &class_compiler;

    named_variable(class_name, false);
    consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        method();
    }
    consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
    emit_byte(OP_POP);

    current_class = current_class->enclosing;
}

static void delete_property(void) {
    variable(false);
    if (match(TOKEN_LEFT_PAREN)) {
        call(false);
    }
    consume(TOKEN_DOT, "Expect '.' before property in deletion target.");
    consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
    uint32_t name = identifier_constant(&parser.previous);

    while (!check(TOKEN_SEMICOLON) && !check(TOKEN_EOF)) {
        if (match(TOKEN_LEFT_PAREN)) {
            emit_varint_instruction(OP_GET_PROPERTY, name);
            call(false);
            consume(TOKEN_DOT, "Expect '.' after call in deletion target.");
        }
        else if (match(TOKEN_DOT)) {
            emit_varint_instruction(OP_GET_PROPERTY, name);
        }
        else {
            error_at_current("Expect '.', '(' or ';' after identifier in deletion target.");
        }
        consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
        name = identifier_constant(&parser.previous);
    }

    emit_varint_instruction(OP_DEL_PROPERTY, name);
}

static void delete_variable(void) {
    Token *name = &parser.previous;
    uint32_t local = resolve_local(current, name);

    if (local == (unsigned)-1) {
        uint32_t upvalue = resolve_upvalue(current, name);
        if (upvalue == (unsigned)-1) {
            // Global.
            uint32_t global = identifier_constant(name);
            emit_varint_instruction(OP_DEL_GLOBAL, global);
            return;
        }
        // Upvalue.
        local = current->upvalues[upvalue].index;
    }
    
    current->locals[local].is_alive = false;
}

static void del_statement(void) {
    consume(TOKEN_IDENTIFIER, "Expect identifier after 'del'.");
    
    if (check(TOKEN_DOT) || check(TOKEN_LEFT_PAREN)) {
        delete_property();
    }
    else {
        delete_variable();
    }

    consume(TOKEN_SEMICOLON, "Expect ';' after deletion target.");
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
    uint8_t set_op;
    if (arg != (unsigned)-1) {
        set_op = OP_SET_LOCAL;
    }
    else {
        arg = identifier_constant(name);
        set_op = OP_SET_GLOBAL;
    }
    emit_varint_instruction(set_op, arg);
    
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
        if (current->type == TYPE_INITIALIZER) {
            error("Can't return a values from an initializer.");
        }

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
    if (match(TOKEN_CLASS)) {
        class_declaration();
    }
    else if (match(TOKEN_FUN)) {
        fun_declaration();
    }
    else if (match(TOKEN_VAR)) {
        var_declaration(true);
    }
    else if (match(TOKEN_VAL)) {
        var_declaration(false);
    }
    else if (match(TOKEN_DEL)) {
        del_statement();
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

void mark_compiler_roots(void) {
    Compiler *compiler = current;
    while (compiler != NULL) {
        mark_object((Obj *)compiler->function);
        compiler = compiler->enclosing;
    }
}
