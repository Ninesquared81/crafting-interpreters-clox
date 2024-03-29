#include <stdlib.h>

#include "line.h"
#include "memory.h"


void init_line_list(LineList *list) {
    list->capacity = 0;
    list->count = 0;
    list->line_numbers = NULL;
    list->line_counts = NULL;
}

void free_line_list(LineList *list) {
    FREE_ARRAY(int, list->line_numbers, list->capacity);
    FREE_ARRAY(int, list->line_counts, list->capacity);
    init_line_list(list);
}

static void new_line(LineList *lines, int line) {
    if (lines->capacity < lines->count + 1) {
        size_t old_capacity = lines->capacity;
        lines->capacity = GROW_CAPACITY(old_capacity);
        lines->line_numbers = GROW_ARRAY(int, lines->line_numbers, old_capacity, lines->capacity);
        lines->line_counts = GROW_ARRAY(int, lines->line_counts, old_capacity, lines->capacity);
    }
    
    lines->line_numbers[lines->count] = line;
    lines->line_counts[lines->count] = 1;
    ++lines->count;
}

void add_line(LineList *lines, int line) {
    if (lines->capacity > 0 && line == lines->line_numbers[lines->count - 1]) {
        ++lines->line_counts[lines->count - 1];
    }
    else {
        new_line(lines, line);
    }

}

int get_line(LineList *lines, size_t index) {
    if (lines->capacity == 0) return -1;
    unsigned int line_index = 0;
    unsigned int count = lines->line_counts[line_index];
    while (index >= count) {
        if (line_index >= lines->count) return -2;
        
        index -= count;
        count = lines->line_counts[++line_index];
    }

    return lines->line_numbers[line_index];
}
    
