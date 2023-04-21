#include <stdlib.h>

#include "chunk.h"
#include "memory.h"
#include "line.h"


void init_chunk(Chunk *chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    init_line_list(&chunk->lines);
    init_value_array(&chunk->constants);
}

void free_chunk(Chunk *chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    free_value_array(&chunk->constants);
    free_line_list(&chunk->lines);
    init_chunk(chunk);
}

void write_chunk(Chunk *chunk, uint8_t byte, int line) {
    if (chunk->capacity < chunk->count + 1) {
        uint32_t old_capacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(old_capacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, old_capacity, chunk->capacity);
    }

    chunk->code[chunk->count] = byte;
    ++chunk->count;

    add_line(&chunk->lines, line);
}

uint32_t add_constant(Chunk *chunk, Value value) {
    write_value_array(&chunk->constants, value);
    return chunk->constants.count - 1;
}

void write_constant(Chunk *chunk, Value value, int line) {
    uint32_t constant = add_constant(chunk, value);
    uint8_t op_code = constant <= 255u ? OP_CONSTANT : OP_CONSTANT_LONG;
    write_chunk(chunk, op_code, line);
    if (op_code == OP_CONSTANT_LONG) {
        for (int i = 4; i > 0; i -= 2) {
            write_chunk(chunk, constant >> 4*i, line);
            constant &= 0xffffff >> (4-i);
        }
    }
    write_chunk(chunk, constant, line);
}
