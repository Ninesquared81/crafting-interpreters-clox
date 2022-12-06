#include <stdio.h>

#include "debug.h"
#include "value.h"


void disassemble_chunk(Chunk *chunk, const char *name) {
    printf("== %s ==\n", name);
    for (int offset = 0; offset < chunk->count;) {
	offset = disassemble_instruction(chunk, offset);
    }
}

static int constant_instruction(const char *name, Chunk *chunk, int offset) {
    uint8_t constant = chunk->code[offset + 1];
    printf("%-16s %d '", name, constant);
    print_value(chunk->constants.values[constant]);
    printf("'\n");
    return offset + 2;
}

static int constant_long_instruction(const char *name, Chunk *chunk, int offset) {
    uint32_t constant = 0;
    for (int i = 1; i <= 3; ++i) {
	constant <<= 8;
	constant |= chunk->code[offset + i];
    }
    printf("%-16s %d '", name, constant);
    print_value(chunk->constants.values[constant]);
    printf("'\n");
    return offset + 4;
}    

static int simple_instruction(const char *name, int offset) {
    printf("%s\n", name);
    return offset + 1;
}

int disassemble_instruction(Chunk *chunk, int offset) {
    printf("%04d ", offset);
    if (offset > 0 && get_line(&chunk->lines, offset) == get_line(&chunk->lines, offset - 1)) {
	printf("   | ");
    }
    else {
	printf("%4d ", get_line(&chunk->lines, offset));
    }
    uint8_t instruction = chunk->code[offset];
    switch (instruction) {
    case OP_CONSTANT:
	return constant_instruction("OP_CONSTANT", chunk, offset);
    case OP_CONSTANT_LONG:
	return constant_long_instruction("OP_CONSTANT_LONG", chunk, offset);
    case OP_RETURN:
	return simple_instruction("OP_RETURN", offset);
    default:
	printf("Unknown opcode %d\n", instruction);
	return offset + 1;
    }
}
    
