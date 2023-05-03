#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "vm.h"

VM vm;

static Value clock_native(int arg_count, Value *args) {
    (void)arg_count; (void)args;
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static void reset_stack(void) {
    vm.stack_top = vm.stack;
    vm.frame_count = 0;
}

static void init_stack(void) {
    vm.stack = ALLOCATE(Value, STACK_SIZE_INIT);
    reset_stack();
}

static void free_stack(void) {
    FREE_ARRAY(Value, vm.stack, vm.stack_capacity);
    vm.stack = NULL;
    vm.stack_capacity = 0;
    reset_stack();
}

static void runtime_error(const char *format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    for (int i = vm.frame_count - 1; i >= 0; --i) {
        CallFrame *frame = &vm.frames[i];
        ObjFunction *function = frame->function;
        size_t instruction = frame->ip - frame->function->chunk.code - 1;
        int line = get_line(&frame->function->chunk.lines, instruction);
        fprintf(stderr, "[line %d] in ", line);
        if (function->name == NULL) {
            fprintf(stderr, "script\n");
        }
        else {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }

    reset_stack();
}

static void define_native(const char *name, NativeFn function, int arity) {
    push(OBJ_VAL(copy_string(name, (int)strlen(name))));
    push(OBJ_VAL(new_native(function, arity)));
    table_set(&vm.globals, STRING_KEY(AS_STRING(vm.stack[0])), vm.stack[1]);
    pop();
    pop();
}

void init_vm(void) {
    init_stack();
    vm.objects = NULL;

    init_table(&vm.globals);
    init_table(&vm.strings);
    init_set(&vm.immutable_globals);

    define_native("clock", clock_native, 0);
}

void free_vm(void) {
    free_table(&vm.globals);
    free_table(&vm.strings);
    free_set(&vm.immutable_globals);
    free_stack();
    
    free_objects();
}

void push(Value value) {
    if (vm.stack == NULL || vm.stack_top >= vm.stack + vm.stack_capacity) {
        size_t old_capacity = vm.stack_capacity;
        vm.stack_capacity = GROW_CAPACITY(old_capacity);
        vm.stack = GROW_ARRAY(Value, vm.stack, old_capacity, vm.stack_capacity);
        vm.stack_top = vm.stack + old_capacity;

        // Make sure slots pointers point into the new allocation.
        // Note: We have to save the offset and use it here, since at this point the slots pointer is invalid.
        for (CallFrame *frame = vm.frames; frame < vm.frames + vm.frame_count; ++frame) {
            frame->slots = vm.stack + frame->slots_offset;
        }
    }
    *vm.stack_top++ = value;
}

Value pop(void) {
    return *--vm.stack_top;
}

static Value popn(uint32_t n) {
    vm.stack_top -= n;
    return *vm.stack_top;
}

static Value peek(int distance) {
    return vm.stack_top[-1 - distance];
}

static bool call(ObjFunction *function, int arg_count) {
    if (arg_count != function->arity) {
        runtime_error("Expected %d arguments but got %d.", function->arity, arg_count);
        return false;
    }

    if (vm.frame_count == FRAMES_MAX) {
        runtime_error("Stack overflow.");
        return false;
    }

    CallFrame *frame = &vm.frames[vm.frame_count++];
    frame->function = function;
    frame->ip = function->chunk.code;
    frame->slots = vm.stack_top - arg_count - 1;
    frame->slots_offset = frame->slots - vm.stack;
    return true;
}

static bool call_value(Value callee, int arg_count) {
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
        case OBJ_FUNCTION:
            return call(AS_FUNCTION(callee), arg_count);
        case OBJ_NATIVE: {
            ObjNative *native = AS_NATIVE(callee);
            if (arg_count != native->arity) {
                runtime_error("Expected %d arguments but got %d.", native->arity, arg_count);
                return false;
            }
            Value result = native->function(arg_count, vm.stack_top - arg_count);
            vm.stack_top -= arg_count + 1;
            push(result);
            return true;
        }
        default:
            break;  // Non-callable object type.
        }
    }
    runtime_error("Can only call functions and classes.");
    return false;
}

static bool is_falsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate(void) {
    ObjString *b = AS_STRING(pop());
    ObjString *a = AS_STRING(pop());
    int length = a->length + b->length;
    char *chars = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    ObjString *result = take_string(chars, length);
    push(OBJ_VAL(result));
}

static InterpretResult run () {
    CallFrame *frame = &vm.frames[vm.frame_count - 1];
    register uint8_t *ip = frame->ip;
    
#define READ_BYTE() (*ip++)
#define READ_BYTES() (ip += 3, (uint32_t)( \
                      (ip[-3] << 16) ^     \
                      (ip[-2] << 8)  ^     \
                      (ip[-1]     )))
#define READ_SHORT() (ip += 2, (uint16_t)((ip[-2] << 8) | ip[-1]))
#define READ_CONSTANT() (frame->function->chunk.constants.values[READ_BYTE()])
#define READ_CONSTANT_LONG() (frame->function->chunk.constants.values[READ_BYTES()])
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define READ_STRING_LONG() AS_STRING(READ_CONSTANT_LONG())

#define BINARY_OP(value_type, op)                               \
    do {                                                        \
        if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) {       \
            runtime_error("Operands must be numbers.");         \
            return INTERPRET_RUNTIME_ERROR;                     \
        }                                                       \
        double b = AS_NUMBER(pop());                            \
        double a = AS_NUMBER(pop());                            \
        push(value_type(a op b));                               \
    } while (false)
    
    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        printf("      ");
        for (Value *slot = vm.stack; slot < vm.stack_top; ++slot) {
            printf("[ ");
            print_value(*slot);
            printf(" ]");
        }
        printf("\n");
        disassemble_instruction(&frame->function->chunk, (int)(ip - frame->function->chunk.code));
#endif
        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
        case OP_CONSTANT: {
            Value constant = READ_CONSTANT();
            push(constant);
            break;
        }
        case OP_CONSTANT_LONG: {
            Value constant = READ_CONSTANT_LONG();
            push(constant);
            break;
        }
        case OP_NIL: push(NIL_VAL); break;
        case OP_TRUE: push(BOOL_VAL(true)); break;
        case OP_FALSE: push(BOOL_VAL(false)); break;
        case OP_POP: pop(); break;
        case OP_POPN: popn(READ_BYTE()); break;
        case OP_POPN_LONG: popn(READ_BYTES()); break;
        case OP_GET_GLOBAL: {
            ObjString *name = READ_STRING();
            Value value;
            if (!table_get(&vm.globals, STRING_KEY(name), &value)) {
                runtime_error("Undefined variable '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            push(value);
            break;
        }
        case OP_GET_GLOBAL_LONG: {
            ObjString *name = READ_STRING_LONG();
            Value value;
            if (!table_get(&vm.globals, STRING_KEY(name), &value)) {
                runtime_error("Undefined variable '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            push(value);
            break;
        }
        case OP_GET_LOCAL: {
            uint8_t slot = READ_BYTE();
            push(frame->slots[slot]);
            break;
        }
        case OP_GET_LOCAL_LONG: {
            uint32_t slot = READ_BYTES();
            push(frame->slots[slot]);
            break;
        }
        case OP_DEFINE_GLOBAL: {
            ObjString *name = READ_STRING();
            Key key = STRING_KEY(name);
            table_set(&vm.globals, key, peek(0));
            pop();
            if (READ_BYTE()) {
                set_delete(&vm.immutable_globals, key);
            }
            else {
                set_add(&vm.immutable_globals, key);
            }
            break;
        }
        case OP_DEFINE_GLOBAL_LONG: {
            ObjString *name = READ_STRING_LONG();
            Key key = STRING_KEY(name);
            table_set(&vm.globals, key, peek(0));
            pop();
            if (READ_BYTE()) {
                set_delete(&vm.immutable_globals, key);
            }
            else {
                set_add(&vm.immutable_globals, key);
            }
            break;
        }
        case OP_SET_GLOBAL: {
            ObjString *name = READ_STRING();
            if (set_check(&vm.immutable_globals, STRING_KEY(name))) {
                runtime_error("Cannot assign to a val.");
                return INTERPRET_RUNTIME_ERROR;
            }
            if (table_set(&vm.globals, STRING_KEY(name), peek(0))) {
                table_delete(&vm.globals, STRING_KEY(name));
                runtime_error("Undefined variable '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_SET_GLOBAL_LONG: {
            ObjString *name = READ_STRING_LONG();
            if (set_check(&vm.immutable_globals, STRING_KEY(name))) {
                runtime_error("Cannot assign to a val.");
                return INTERPRET_RUNTIME_ERROR;
            }
            if (table_set(&vm.globals, STRING_KEY(name), peek(0))) {
                table_delete(&vm.globals, STRING_KEY(name));
                runtime_error("Undefined variable '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_SET_LOCAL: {
            uint8_t slot = READ_BYTE();
            frame->slots[slot] = peek(0);
            break;
        }
        case OP_SET_LOCAL_LONG: {
            uint32_t slot = READ_BYTES();
            frame->slots[slot] = peek(0);
            break;
        }
        case OP_EQUAL: {
            Value b = pop();
            Value a = pop();
            push(BOOL_VAL(values_equal(a, b)));
            break;
        }
        case OP_GREATER:
            BINARY_OP(BOOL_VAL, >);
            break;
        case OP_LESS:
            BINARY_OP(BOOL_VAL, <);
            break;
        case OP_ADD: {
            if (IS_STRING(peek(0)) || IS_STRING(peek(1))) {
                ObjString *a = to_string(pop());
                ObjString *b = to_string(pop());
                push(OBJ_VAL((Obj *)b));
                push(OBJ_VAL((Obj *)a));
                concatenate();
            }
            else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
                double b = AS_NUMBER(pop());
                double a = AS_NUMBER(pop());
                push(NUMBER_VAL(a + b));
            }
            else {
                runtime_error("Operands must be two numbers or two strings.");
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_SUBTRACT:
            BINARY_OP(NUMBER_VAL, -);
            break;
        case OP_MULTIPLY:
            BINARY_OP(NUMBER_VAL, *);
            break;
        case OP_DIVIDE:
            BINARY_OP(NUMBER_VAL, /);
            break;
        case OP_NOT:
            push(BOOL_VAL(is_falsey(pop())));
            break;
        case OP_NEGATE:
            if (!IS_NUMBER(peek(0))) {
                runtime_error("Operand must be a number.");
                return INTERPRET_RUNTIME_ERROR;
            }
            push(NUMBER_VAL(-AS_NUMBER(pop())));
            break;
        case OP_PRINT: {
            // TODO: This could be changed to a call to `to_string()` since we already defined it.
            // i.e.
            // printf("%s\n", to_sting(pop()));
            print_value(pop());
            printf("\n");
            break;
        }
        case OP_JUMP: {
            uint16_t offset = READ_SHORT();
            ip += offset;
            break;
        }
        case OP_JUMP_IF_FALSE: {
            uint16_t offset = READ_SHORT();
            if (is_falsey(peek(0))) ip += offset;
            break;
        }
        case OP_LOOP: {
            uint16_t offset = READ_SHORT();
            ip -= offset;
            break;
        }
        case OP_CALL: {
            int arg_count = READ_BYTE();
            frame->ip = ip;
            if (!call_value(peek(arg_count), arg_count)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            frame = &vm.frames[vm.frame_count - 1];
            ip = frame->ip;
            break;
        }
        case OP_CALL_LONG: {
            int arg_count = READ_BYTES();
            frame->ip = ip;
            if (!call_value(peek(arg_count), arg_count)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            frame = &vm.frames[vm.frame_count - 1];
            ip = frame->ip;
            break;
        }
        case OP_RETURN: {
            Value result = pop();
            vm.frame_count--;
            if (vm.frame_count == 0) {
                pop();
                return INTERPRET_OK;
            }

            vm.stack_top = frame->slots;
            push(result);
            frame = &vm.frames[vm.frame_count - 1];
            ip = frame->ip;
            break;
        }
        }
    }

#undef READ_BYTE
#undef READ_BYTES
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_CONSTANT_LONG
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(const char *source) {
    ObjFunction *function = compile(source);
    if (function == NULL) return INTERPRET_COMPILE_ERROR;

    push(OBJ_VAL(function));
    call(function, 0);

    return run();
}
