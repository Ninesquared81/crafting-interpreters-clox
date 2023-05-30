#include <stdio.h>

#include "debug.h"
#include "object.h"
#include "value.h"


void disassemble_chunk(Chunk *chunk, const char *name) {
    printf("== %s ==\n", name);
    for (int offset = 0; offset < chunk->count;) {
        offset = disassemble_instruction(chunk, offset);
    }
}

static uint32_t get_uint24(Chunk *chunk, int offset) {
    uint32_t constant = 0;
    for (int i = 1; i <= 3; ++i) {
        constant <<= 8;
        constant |= chunk->code[offset + i];
    }
    return constant;
}

static int define_instruction(const char *name, Chunk *chunk, int offset) {
    uint8_t constant = chunk->code[offset + 1];
    uint8_t is_mutable = chunk->code[offset + 2];
    printf("%-16s %d '", name, constant);
    print_value(chunk->constants.values[constant]);
    printf("' %d\n", is_mutable);
    return offset + 3;
}

static int define_long_instruction(const char *name, Chunk *chunk, int offset) {
    uint32_t constant = get_uint24(chunk, offset);
    uint8_t is_mutable = chunk->code[offset + 4];
    printf("%-16s %d '", name, constant);
    print_value(chunk->constants.values[constant]);
    printf("' %d\n", is_mutable);
    return offset + 5;
}

static int constant_instruction(const char *name, Chunk *chunk, int offset) {
    uint8_t constant = chunk->code[offset + 1];
    printf("%-16s %d '", name, constant);
    print_value(chunk->constants.values[constant]);
    printf("'\n");
    return offset + 2;
}

static int constant_long_instruction(const char *name, Chunk *chunk, int offset) {
    uint32_t constant = get_uint24(chunk, offset);
    printf("%-16s %d '", name, constant);
    print_value(chunk->constants.values[constant]);
    printf("'\n");
    return offset + 4;
}    

static int simple_instruction(const char *name, int offset) {
    printf("%s\n", name);
    return offset + 1;
}

static int byte_instruction(const char *name, Chunk *chunk, int offset) {
    uint8_t slot = chunk->code[offset + 1];
    printf("%-16s %4d\n", name, slot);
    return offset + 2;
}

static int byte_long_instruction(const char *name, Chunk *chunk, int offset) {
    uint32_t slot = get_uint24(chunk, offset);
    printf("%-16s %4d\n", name, slot);
    return offset + 4;
}

static int jump_instruction(const char *name, int sign, Chunk *chunk, int offset) {
    uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);
    jump |= chunk->code[offset + 2];
    printf("%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
    return offset + 3;
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
    case OP_NIL:
        return simple_instruction("OP_NIL", offset);
    case OP_TRUE:
        return simple_instruction("OP_TRUE", offset);
    case OP_FALSE:
        return simple_instruction("OP_FALSE", offset);
    case OP_POP:
        return simple_instruction("OP_POP", offset);
    case OP_POPN:
        return byte_instruction("OP_POPN", chunk, offset);
    case OP_POPN_LONG:
        return byte_long_instruction("OP_POPN_LONG", chunk, offset);
    case OP_GET_GLOBAL:
        return constant_instruction("OP_GET_GLOBAL", chunk, offset);
    case OP_GET_GLOBAL_LONG:
        return constant_long_instruction("OP_GET_GLOBAL_LONG", chunk, offset);
    case OP_GET_LOCAL:
        return byte_instruction("OP_GET_LOCAL", chunk, offset);
    case OP_GET_LOCAL_LONG:
        return byte_long_instruction("OP_GET_LOCAL_LONG", chunk, offset);
    case OP_DEFINE_GLOBAL:
        return define_instruction("OP_DEFINE_GLOBAL", chunk, offset);
    case OP_DEFINE_GLOBAL_LONG:
        return define_long_instruction("OP_DEFINE_GLOBAL_LONG", chunk, offset);
    case OP_SET_GLOBAL:
        return constant_instruction("OP_SET_GLOBAL", chunk, offset);
    case OP_SET_GLOBAL_LONG:
        return constant_long_instruction("OP_SET_GLOBAL_LONG", chunk, offset);
    case OP_SET_LOCAL:
        return byte_instruction("OP_SET_LOCAL", chunk, offset);
    case OP_SET_LOCAL_LONG:
        return byte_long_instruction("OP_SET_LOCAL_LONG", chunk, offset);
    case OP_GET_UPVALUE:
        return byte_instruction("OP_GET_UPVALUE", chunk, offset);
    case OP_GET_UPVALUE_LONG:
        return byte_long_instruction("OP_GET_UPVALUE_LONG", chunk, offset);
    case OP_SET_UPVALUE:
        return byte_instruction("OP_SET_UPVALUE", chunk, offset);
    case OP_SET_UPVALUE_LONG:
        return byte_long_instruction("OP_SET_UPVALUE_LONG", chunk, offset);
    case OP_GET_PROPERTY:
        return constant_instruction("OP_GET_PROPERTY", chunk, offset);
    case OP_GET_PROPERTY_LONG:
        return constant_long_instruction("OP_GET_PROPERTY_LONG", chunk, offset);
    case OP_SET_PROPERTY:
        return constant_instruction("OP_SET_PROPERTY", chunk, offset);
    case OP_SET_PROPERTY_LONG:
        return constant_long_instruction("OP_SET_PROPERTY_LONG", chunk, offset);
    case OP_DEL_PROPERTY:
        return constant_instruction("OP_DEL_PROPERTY", chunk, offset);
    case OP_DEL_PROPERTY_LONG:
        return constant_long_instruction("OP_DEL_PROPERY_LONG", chunk, offset);
    case OP_DEL_GLOBAL:
        return constant_instruction("OP_DEL_GLOBAL", chunk, offset);
    case OP_DEL_GLOBAL_LONG:
        return constant_long_instruction("OP_DEL_GLOBAL_LONG", chunk, offset);
    case OP_EQUAL:
        return simple_instruction("OP_EQUAL", offset);
    case OP_GREATER:
        return simple_instruction("OP_GREATER", offset);
    case OP_LESS:
        return simple_instruction("OP_LESS", offset);
    case OP_ADD:
        return simple_instruction("OP_ADD", offset);
    case OP_SUBTRACT:
        return simple_instruction("OP_SUBTRACT", offset);
    case OP_MULTIPLY:
        return simple_instruction("OP_MULTIPLY", offset);
    case OP_DIVIDE:
        return simple_instruction("OP_DIVIDE", offset);
    case OP_NOT:
        return simple_instruction("OP_NOT", offset);
    case OP_NEGATE:
        return simple_instruction("OP_NEGATE", offset);
    case OP_INPUT:
        return simple_instruction("OP_INPUT", offset);
    case OP_PRINT:
        return simple_instruction("OP_PRINT", offset);
    case OP_JUMP:
        return jump_instruction("OP_JUMP", 1, chunk, offset);
    case OP_JUMP_IF_FALSE:
        return jump_instruction("OP_JUMP_IF_FALSE", 1, chunk, offset);
    case OP_LOOP:
        return jump_instruction("OP_LOOP", -1, chunk, offset);
    case OP_CALL:
        return byte_instruction("OP_CALL", chunk, offset);
    case OP_CALL_LONG:
        return byte_long_instruction("OP_CALL_LONG", chunk, offset);
    case OP_CLOSURE:
    case OP_CLOSURE_LONG: {
        offset++;
        ulong constant = chunk->code[offset++];
        const char *name = "OP_CLOSURE";
        if (instruction == OP_CLOSURE_LONG) {
            constant <<= 16;
            constant ^= (chunk->code[offset++]) << 8;
            constant ^= (chunk->code[offset++]);
            name = "OP_CLOSURE_LONG";
        }
        printf("%-16s %4lu ", name, constant);
        print_value(chunk->constants.values[constant]);
        printf("\n");

        ObjFunction *function = AS_FUNCTION(chunk->constants.values[constant]);
        for (ulong j = 0; j < function->upvalue_count; ++j) {
            uint8_t byte1 = chunk->code[offset++];
            bool is_long = byte1 >> 7;
            int is_local = byte1 & 1;
            ulong index = chunk->code[offset++];
            if (is_long) {
                index <<= 16;
                index ^= (chunk->code[offset++]) << 8;
                index ^= (chunk->code[offset++]);
            }
            printf("%04d      |                      %s %lu\n",
                   offset - 2, is_local ? "local" : "upvalue", index);
        }

        return offset;
    }
    case OP_CLOSE_UPVALUE:
        return simple_instruction("OP_CLOSE_UPVALUE", offset);
    case OP_RETURN:
        return simple_instruction("OP_RETURN", offset);
    case OP_CLASS:
        return constant_instruction("OP_CLASS", chunk, offset);
    case OP_CLASS_LONG:
        return constant_long_instruction("OP_CLASS_LONG", chunk,offset);
    default:
        printf("Unknown opcode %d\n", instruction);
        return offset + 1;
    }
}
