#ifndef CLOX_SET_H
#define CLOX_SET_H

#include "table.h"

typedef Table Set;

void init_set(Set *set);
void free_set(Set *set);
bool set_check(Set *set, Key key);
bool set_add(Set *set, Key key);
bool set_delete(Set *set, Key key);

#endif
