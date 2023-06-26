#ifndef CLOX_CHUNK_H
#define CLOX_CHUNK_H

#include "common.h"
#include "value.h"
#include "line.h"

#define IS_LONG_INSTRUCTION(opcode)             \
    ((opcode) == OP_CONSTANT_LONG               \
     || (opcode) == OP_GET_GLOBAL_LONG          \
     || (opcode) == OP_GET_LOCAL_LONG           \
     || (opcode) == OP_DEFINE_GLOBAL_LONG       \
     || (opcode) == OP_SET_GLOBAL_LONG          \
     || (opcode) == OP_SET_LOCAL_LONG           \
     || (opcode) == OP_POPN_LONG                \
     || (opcode) == OP_CALL_LONG                \
     || (opcode) == OP_CLOSURE_LONG             \
     || (opcode) == OP_GET_UPVALUE_LONG         \
     || (opcode) == OP_SET_UPVALUE_LONG         \
     || (opcode) == OP_CLASS_LONG               \
     || (opcode) == OP_GET_PROPERTY_LONG        \
     || (opcode) == OP_SET_PROPERTY_LONG        \
     || (opcode) == OP_DEL_PROPERTY_LONG        \
     || (opcode) == OP_DEL_GLOBAL_LONG          \
     || (opcode) == OP_METHOD_LONG              \
     || (opcode) == OP_INVOKE_LONG              \
     || (opcode) == OP_GET_SUPER_LONG           \
     || (opcode) == OP_SUPER_INVOKE_LONG        \
     || (opcode) == OP_ARRAY_LONG               \
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
    OP_GET_LOCAL,
    OP_GET_LOCAL_LONG,
    OP_SET_LOCAL,
    OP_SET_LOCAL_LONG,
    OP_GET_GLOBAL,
    OP_GET_GLOBAL_LONG,
    OP_DEFINE_GLOBAL,
    OP_DEFINE_GLOBAL_LONG,
    OP_SET_GLOBAL,
    OP_SET_GLOBAL_LONG,
    OP_GET_UPVALUE,
    OP_GET_UPVALUE_LONG,
    OP_SET_UPVALUE,
    OP_SET_UPVALUE_LONG,
    OP_GET_PROPERTY,
    OP_GET_PROPERTY_LONG,
    OP_SET_PROPERTY,
    OP_SET_PROPERTY_LONG,
    OP_DEL_PROPERTY,
    OP_DEL_PROPERTY_LONG,
    OP_DEL_GLOBAL,
    OP_DEL_GLOBAL_LONG,
    OP_GET_SUPER,
    OP_GET_SUPER_LONG,
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
    OP_ARRAY,
    OP_ARRAY_LONG,
    OP_INDEX,
    OP_CALL,
    OP_CALL_LONG,
    OP_INVOKE,
    OP_INVOKE_LONG,
    OP_SUPER_INVOKE,
    OP_SUPER_INVOKE_LONG,
    OP_CLOSURE,
    OP_CLOSURE_LONG,
    OP_CLOSE_UPVALUE,
    OP_RETURN,
    OP_CLASS,
    OP_CLASS_LONG,
    OP_INHERIT,
    OP_METHOD,
    OP_METHOD_LONG,
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
