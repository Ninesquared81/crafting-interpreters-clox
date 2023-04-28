#ifndef CLOX_VM_H
#define CLOX_VM_H

#include "chunk.h"
#include "set.h"
#include "table.h"
#include "value.h"

//#define STACK_MAX 256

typedef struct {
    Chunk *chunk;
    uint8_t *ip;
    Value *stack;
    Value *stack_top;
    size_t stack_capacity;
    Table globals;
    Set immutable_globals;
    Table strings;
    Obj *objects;
} VM;

typedef enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
} InterpretResult;

extern VM vm;

void init_vm(void);
void free_vm(void);
InterpretResult interpret(const char *source);
void push(Value value);
Value pop(void);

#endif
