#include <stdarg.h>
#include <stdio.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "vm.h"

VM vm;

static void reset_stack(void) {
    vm.stack_top = vm.stack;
}

static void runtume_error(const char *format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    size_t instruction = vm.ip - vm.chunk->code - 1;
    int line = vm.chunk->lines[instruction];
    fprintf(stderr, "[line %d] in script\n", line);
    reset_stack();
}

void init_vm(void) {
    reset_stack();
}

void free_vm(void) {
}

static InterpretResult run () {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define READ_CONSTANT_LONG() (*vm.ip += 3, vm.chunk->constants.values[  \
                                  ((uint32_t)(*vm.ip-3) << 16) |        \
                                  ((uint32_t)(*vm.ip-2) << 8)  |        \
                                  (uint32_t)(*vm.ip-1)])

#define BINARY_OP(value_type, op)                               \
    do {                                                        \
        if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) {       \
            runtime_error("Operands must be numbers.");         \
            return INTERPRET_RUNTIME_ERROR;                     \
        }                                                       \
        double b = pop();                                       \
        double a = pop();                                       \
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
        disassemble_instruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
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
        case OP_ADD:
            BINARY_OP(NUMBER_VAL, +);
            break;
        case OP_SUBTRACT:
            BINARY_OP(NUMBER_VAL, -);
            break;
        case OP_MULTIPLY:
            BINARY_OP(NUMBER_VAL, *);
            break;
        case OP_DIVIDE:
            BINARY_OP(NUMBER_VAL, /);
            break;
        case OP_NEGATE:
            if (!IS_NUMBER(peek(0))) {
                runtime_error("Operand must be a number.");
                return INTERPRET_RUNTIME_ERROR;
            }
            push(NUMBER_VAL(-AS_NUMBER(pop())));
            break;
        case OP_RETURN: {
            print_value(pop());
            printf("\n");
            return INTERPRET_OK;
        }
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
#undef READ_CONSTANT_LONG
#undef BINARY_OP
}

InterpretResult interpret(const char *source) {
    Chunk chunk;
    init_chunk(&chunk);
    
    if (!compile(source, &chunk)) {
        free_chunk(&chunk);
        return INTERPRET_COMPILE_ERROR;
    }
    
    vm.chunk = &chunk;
    vm.ip = vm.chunk->code;
    
    InterpretResult result = run();

    free_chunk(&chunk);
    return result;
}

void push(Value value) {
    *vm.stack_top++ = value;
}

Value pop(void) {
    return *--vm.stack_top;
}

static Value peek(int distance) {
    return vm.stack_top[-1 - distance];
}
