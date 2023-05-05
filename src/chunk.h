#ifndef CLOX_CHUNK_H
#define CLOX_CHUNK_H

#include "common.h"
#include "value.h"
#include "line.h"

#define IS_LONG_INSTRUCTION(opcode) (                                   \
        (opcode) == OP_CONSTANT_LONG      ||                            \
        (opcode) == OP_GET_GLOBAL_LONG    ||                            \
        (opcode) == OP_GET_LOCAL_LONG     ||                            \
        (opcode) == OP_DEFINE_GLOBAL_LONG ||                            \
        (opcode) == OP_SET_GLOBAL_LONG    ||                            \
        (opcode) == OP_SET_LOCAL_LONG     ||                            \
        (opcode) == OP_POPN_LONG          ||                            \
        (opcode) == OP_CALL_LONG                                        \
        )

typedef enum {
    OP_CONSTANT,
    OP_CONSTANT_LONG,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_POP,
    OP_POPN,
    OP_POPN_LONG,
    OP_GET_GLOBAL,
    OP_GET_GLOBAL_LONG,
    OP_GET_LOCAL,
    OP_GET_LOCAL_LONG,
    OP_DEFINE_GLOBAL,
    OP_DEFINE_GLOBAL_LONG,
    OP_SET_GLOBAL,
    OP_SET_GLOBAL_LONG,
    OP_SET_LOCAL,
    OP_SET_LOCAL_LONG,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_NEGATE,
    OP_INPUT,
    OP_PRINT,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_LOOP,
    OP_CALL,
    OP_CALL_LONG,
    OP_RETURN,
} OpCode;

typedef struct {
    int count;
    int capacity;
    uint8_t *code;
    LineList lines;
    ValueArray constants;
} Chunk;

void init_chunk(Chunk *chunk);
void free_chunk(Chunk *chunk);
void write_chunk(Chunk *chunk, uint8_t byte, int line);
uint32_t add_constant(Chunk *chunk, Value value);
void write_constant(Chunk *chunk, Value value, int line);

#endif
