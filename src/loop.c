#include "common.h"
#include "loop.h"
#include "memory.h"

void init_jump_array(JumpArray *array) {
    array->offsets = NULL;
    array->count = 0;
    array->capacity = 0;
}

void free_jump_array(JumpArray *array) {
    FREE_ARRAY(uint16_t*, array->offsets, array->capacity);
    init_jump_array(array);
}

void push_jump_array(JumpArray *array, uint16_t *offset) {
    if (array->count + 1 > array->capacity) {
        size_t old_capacity = array->capacity;
        array->capacity = GROW_CAPACITY(array->capacity);
        GROW_ARRAY(uint16_t*, array->offsets, old_capacity, array->capacity);
    }
    array->offsets[array->count++] = offset;
}

void init_loop_stack(LoopStack *stack) {
    stack->start = NULL;
    stack->top = NULL;
    stack->capacity = 0;
}

void free_loop_stack(LoopStack *stack) {
    FREE_ARRAY(Loop, stack->start, stack->capacity);
    init_loop_stack(stack);
}

void push_loop_stack(LoopStack *stack, Loop loop) {
    if (stack->top == stack->start + stack->capacity) {
        size_t old_capacity = stack->capacity;
        stack->capacity = GROW_CAPACITY(old_capacity);
        GROW_ARRAY(Loop, stack->start, old_capacity, stack->capacity);
    }
    *stack->top++ = loop;
}

Loop pop_loop_stack(LoopStack *stack) {
    return *--stack->top;
}

bool push_break(LoopStack *stack, uint16_t *offset) {
    if (stack->top == stack->start) return false;

    push_jump_array(&stack->top[-1].breaks, offset);
    return true;
}

bool push_continue(LoopStack *stack, uint16_t *offset) {
    if (stack->top == stack->start) return false;

    push_jump_array(&stack->top[-1].continues, offset);
    return true;
}
