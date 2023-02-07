#ifndef CLOX_TABLE_H
#define CLOX_TABLE_H

#include "common.h"
#include "value.h"

typedef enum {
    // Dummy keys.
    KEY_EMPTY,
    KEY_TOMBSTONE,

    // Actual keys.
    KEY_STRING,
    KEY_NUMBER,
    KEY_BOOL,
    KEY_NIL,
} KeyType;

typedef struct {
    KeyType type;
    union {
        ObjString *string;
        double number;
        bool boolean;
    } as;
} Key;

typedef struct {
    Key key;
    Value value;
} Entry;

typedef struct {
    int count;
    int capacity;
    Entry *entries;
} Table;

void init_table(Table *table);
void free_table(Table *table);
bool table_get(Table *table, Key key, Value *value);
bool table_set(Table *table, Key key, Value value);
bool table_delete(Table *table, Key key);
void table_add_all(Table *from, Table *to);
ObjString *table_find_string(Table *table, const char *chars, int length, uint32_t hash);

#endif
