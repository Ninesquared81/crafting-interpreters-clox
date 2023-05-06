#ifndef CLOX_VM_H
#define CLOX_VM_H

#include "common.h"
#include "object.h"
#include "set.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
//#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)
#define STACK_SIZE_INIT (FRAMES_MAX * UINT8_COUNT)

typedef struct {
    ObjClosure *closure;
    uint8_t *ip;
    Value *slots;
    ptrdiff_t slots_offset;
} CallFrame;

typedef struct {
    CallFrame frames[FRAMES_MAX];
    int frame_count;

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

void runtime_error(const char *format, ...);

#endif
