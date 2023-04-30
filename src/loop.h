#ifndef CLOX_LOOP_H
#define CLOX_LOOP_H

#include <stddef.h>

typedef struct {
    uint16_t **offsets;
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

void init_jump_array(JumpArray *array);
void free_jump_array(JumpArray *array);
void push_jump_array(JumpArray *array, uint16_t *offset);

void init_loop_stack(LoopStack *stack);
void free_loop_stack(LoopStack *stack);
void push_loop_stack(LoopStack *stack, Loop loop);
Loop pop_loop_stack(LoopStack *stack);

bool push_break(LoopStack *stack, uint16_t *offset);
bool push_continue(LoopStack *stack, uint16_t *offset);

#endif
