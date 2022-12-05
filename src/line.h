#ifndef CLOX_LINE_H
#define CLOX_LINE_H

typedef struct {
    int capacity;
    int count;
    int *line_numbers;
    int *line_counts;
} LineList;


void init_line_list(LineList *list);
void free_line_list(LineList *list);

void add_line(LineList *lines, int line);
int get_line(LineList *lines, int index);

#endif
