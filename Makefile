CC = gcc

CFLAGS = -Wall -Wextra -Werror -pedantic -g -std=c11 -D__USE_MINGW_ANSI_STDIO=1 -DDEBUG_PRINT__CODE

sources = $(wildcard src/*.c)
objects = $(patsubst src/%.c,bin/%.o,$(sources))

out = bin/clox

.PHONY: all clean rebuild

all: $(out)

clean:
	rm ./bin/*

rebuild: clean
	$(MAKE) all

# Dependencies
bin/vm.o : src/natives.h src/chunk.h
bin/debug.o : src/chunk.h
bin/compiler.o : src/scanner.h src/chunk.h
bin/object.o : src/object.h

bin/%.o : src/%.c src/common.h
	$(COMPILE.c) $(OUTPUT_OPTION) $<

$(objects) : | bin

bin :
	mkdir bin

$(out) : $(objects)
	$(CC) $(CFLAGS) -o $(out) $(objects)
