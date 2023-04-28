#include "set.h"
#include "table.h"

void init_set(Set *set) {
    init_table(set);
}

void free_set(Set *set) {
    free_table(set);
}

bool set_check(Set *set, Key key) {
    Value dummy = NIL_VAL;
    return table_get(set, key, &dummy);
}

bool set_add(Set *set, Key key) {
    return table_set(set, key, NIL_VAL);
}

bool set_delete(Set *set, Key key) {
    return table_delete(set, key);
}
