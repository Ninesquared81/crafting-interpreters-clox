#include <stdio.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"

typedef struct {
    Token current;
    Token previous;
} Parser;

Parser parser;

static void advance(void) {
    parser.previous = parser.current;
    for (;;) {
        parser.current = scan_token();
        if (parser.current.type != TOKEN_ERROR) break;
        
        error_at_current(parser.current.start);
    }
}

bool compile(const char *source, Chunk *chunk) {
    init_scanner(source);
    advance();
    expression();
    consume(TOKEN_EOF, "Expect end of expression.");
}
