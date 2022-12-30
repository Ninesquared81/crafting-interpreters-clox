#include <stdio.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "vm.h"

VM vm;

static void reset_stack() {
    vm.stack_top = vm.stack;
}

void init_vm(void) {
    reset_stack();
}

void free_vm(void) {
}

static InterpretResult run () {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define READ_CONSTANT_LONG() (*vm.ip += 3, vm.chunk->constants.values[    \
                  ((uint32_t)(*vm.ip-3) << 16) |    \
                  ((uint32_t)(*vm.ip-2) << 8)  |    \
                  (uint32_t)(*vm.ip-1)])
#define BINARY_OP(op) \
    do {  \
    double b = pop();  \
    double a = pop();  \
    push(a op b);       \
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
        BINARY_OP(+);
        break;
    case OP_SUBTRACT:
        BINARY_OP(-);
        break;
    case OP_MULTIPLY:
        BINARY_OP(*);
        break;
    case OP_DIVIDE:
        BINARY_OP(/);
        break;
    case OP_NEGATE:
        push(-pop());
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
    compile(source);
    (void)run;
    return INTERPRET_OK;
}

void push(Value value) {
    *vm.stack_top++ = value;
}

Value pop(void) {
    return *--vm.stack_top;
}
