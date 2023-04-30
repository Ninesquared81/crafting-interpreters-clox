#ifndef CLOX_LOOP_H
#define CLOX_LOOP_H

#include <stddef.h>

#define NEW_JUMP_ARRAY ((JumpArray){.offsets = NULL, .count = 0, .capacity = 0})
#define NEW_LOOP ((Loop){.continues = NEW_JUMP_ARRAY, .breaks = NEW_JUMP_ARRAY})

typedef struct {
    int *offsets;
    size_t count;
    size_t capacity;
} JumpArray;

typedef struct {
    JumpArray breaks;
    JumpArray continues;
} Loop;

typedef struct {
    Loop *start;
    Loop *top;
    size_t capacity;
} LoopStack;

Loop new_loop(void);

void init_jump_array(JumpArray *array);
void free_jump_array(JumpArray *array);
void push_jump_array(JumpArray *array, int offset);

void init_loop_stack(LoopStack *stack);
void free_loop_stack(LoopStack *stack);
void push_loop_stack(LoopStack *stack, Loop loop);
Loop pop_loop_stack(LoopStack *stack);

bool push_break(LoopStack *stack, int offset);
bool push_continue(LoopStack *stack, int offset);

#endif
