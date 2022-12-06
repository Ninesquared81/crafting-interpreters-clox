#include "common.h"
#include "chunk.h"
#include "debug.h"


int main(int argc, const char *argv[]) {
    (void)argc; (void)argv;
    Chunk chunk;
    init_chunk(&chunk);/*
    int constant = add_constant(&chunk, 1.2);
    write_chunk(&chunk, OP_CONSTANT, 123);
    write_chunk(&chunk, constant, 123);*/
    write_constant(&chunk, 1.2, 2);
    write_constant(&chunk, 2.4, 123);
    write_chunk(&chunk, OP_RETURN, 123);

    disassemble_chunk(&chunk, "test chunk");
    free_chunk(&chunk);
    return 0;
}
