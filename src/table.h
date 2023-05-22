#ifndef CLOX_TABLE_H
#define CLOX_TABLE_H

#include "common.h"
#include "value.h"

#define EMPTY_KEY         ((Key){.type = KEY_EMPTY})
#define TOMBSTONE_KEY     ((Key){.type = KEY_TOMBSTONE})

#define STRING_KEY(value) ((Key){.type = KEY_STRING, .as.string = (value)})
#define NUMBER_KEY(value) ((Key){.type = KEY_NUMBER, .as.number = (value)})
#define BOOL_KEY(value)   ((Key){.type = KEY_BOOL, .as.boolean = (value)})
#define NIL_KEY           ((Key){.type = KEY_NIL})

#define IS_STRING_KEY(key) ((key).type == KEY_STRING)
#define IS_NUMBER_KEY(key) ((key).type == KEY_NUMBER)
#define IS_BOOL_KEY(key) ((key).type == KEY_BOOL)
#define IS_NIL_KEY(key) ((key).type == KEY_NIL)

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

Value key_as_value(Key key);

void init_table(Table *table);
void free_table(Table *table);
bool table_get(Table *table, Key key, Value *value);
bool table_set(Table *table, Key key, Value value);
bool table_delete(Table *table, Key key);
void table_add_all(Table *from, Table *to);
ObjString *table_find_string(Table *table, const char *chars, int length, uint32_t hash);
void table_remove_white(Table *table);
void mark_table(Table *table);

#endif
