#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define TABLE_MAX_LOAD 0.75

void init_table(Table *table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void free_table(Table *table) {
    FREE_ARRAY(Entry, table->entries, table->capacity);
    init_table(table);
}

static uint32_t hash_of(Key key) {
    switch (key.type) {
    case KEY_STRING: return key.as.string->hash;
    case KEY_NUMBER: return HASH_NUMBER(key);
    case KEY_BOOL:   return HASH_BOOL(key);
    case KEY_NIL:    return HASH_NIL;
    default:         return 0;  // Unreachable.
    }
}

static bool compare_keys(Key a, Key b) {
    if (a.type != b.type) return false;

    switch (a.type) {
    case KEY_STRING: return a.as.string == b.as.string;
    case KEY_NUMBER: return a.as.number == b.as.number;
    case KEY_BOOL:   return a.as.boolean == b.as.boolean;
    case KEY_NIL:    return true;
    default:         return false;  // Unreachable
    }
}

static Entry *find_entry(Entry *entries, int capacity, Key key) {
    uint32_t index = hash_of(key) % capacity;
    Entry *tombstone = NULL;

    for (;;) {
        Entry *entry = &entries[index];
        switch (entry->key.type) {
        case KEY_EMPTY:
            // Empty entry.
            return tombstone != NULL ? tombstone : entry;
        case KEY_TOMBSTONE:
            // We found a tombstone.
            tombstone = entry;
            break;
        default:
            if (compare_keys(entry->key, key)) {
                // We found the key.
                return entry;
            }
        }

        index = (index + 1) % capacity;
    }
}

static void adjust_capacity(Table *table, int capacity) {
    Entry *entries = ALLOCATE(Entry, capacity);
    for (int i = 0; i < capacity; i++) {
        entries[i].key.type = KEY_EMPTY;
        entries[i].value = NIL_VAL;
    }

    table->count = 0;
    for (int i = 0; i < table->capacity; i++) {
        Entry *entry = &table->entries[i];
        if (IS_UNOCCUPIED(entry->key)) continue;
        Entry *dest = find_entry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }

    FREE_ARRAY(Entry, table->entries, table->capacity);
    table->entries = entries;
    table->capacity = capacity;
}

bool table_get(Table *table, Key key, Value *value) {
    if (table->count == 0) return false;

    Entry *entry = find_entry(table->entries, table->capacity, key);
    if (IS_UNOCCUPIED(entry->key)) return false;

    *value = entry->value;
    return true;
}

bool table_set(Table *table, Key key, Value value) {
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        int capacity = GROW_CAPACITY(table->capacity);
        adjust_capacity(table, capacity);
    }

    Entry *entry = find_entry(table->entries, table->capacity, key);
    bool is_new_key = IS_UNOCCUPIED(entry->key);
    if (entry->key.type == KEY_EMPTY) table->count++;

    entry->key = key;
    entry->value = value;
    return is_new_key;
}

bool table_delete(Table *table, Key key) {
    if (table->count == 0) return false;

    // Find the entry.
    Entry *entry = find_entry(table->entries, table->capacity, key);
    if (IS_UNOCCUPIED(entry->key)) return false;

    // Place a tombstone in the entry.
    entry->key.type = KEY_TOMBSTONE;
    entry->value = BOOL_VAL(true);
    return true;
}

void table_add_all(Table *from, Table *to) {
    for (int i = 0; i < from->capacity; i++) {
        Entry *entry = &from->entries[i];
        if (!IS_UNOCCUPIED(entry->key)) {
            table_set(to, entry->key, entry->value);
        }
    }
}

ObjString *table_find_string(Table *table, const char *chars, int length, uint32_t hash) {
    if (table->count == 0) return NULL;

    uint32_t index = hash % table->capacity;
    for (;;) {
        Entry *entry = &table->entries[index];
        if (entry->key.type == KEY_EMPTY) {
            // Stop if we find an empty non-tombstone entry.
            return NULL;
        }
        else if (entry->key.type == KEY_STRING
                 && entry->key.as.string->length == length
                 && entry->key.as.string->hash == hash
                 && memcmp(entry->key.as.string->chars, chars, length) == 0) {
            // We found it.
            return entry->key.as.string;
        }
        
        index = (index + 1) % table->capacity;
    }
}

Value key_as_value(Key key) {
    switch (key.type) {
    case KEY_STRING:
        return OBJ_VAL(key.as.string);
    case KEY_NUMBER:
        return NUMBER_VAL(key.as.number);
    case KEY_BOOL:
        return BOOL_VAL(key.as.boolean);
    case KEY_NIL:
        return NIL_VAL;
    default:  // Dummy key types.
        return NIL_VAL;
    }
}

Key key_from_value(Value value) {
    Key key;
    switch (value.type) {
    case VAL_NUMBER:
        key.type = KEY_NUMBER;
        key.as.number = AS_NUMBER(value);
        break;
    case VAL_BOOL:
        key.type = KEY_BOOL;
        key.as.boolean = AS_BOOL(value);
        break;
    case VAL_NIL:
        key.type = KEY_NIL;
        break;
    case VAL_OBJ:
        if (IS_STRING(value)) {
            key.type = KEY_STRING;
            key.as.string = AS_STRING(value);
        }
        else {
            key.type = KEY_EMPTY;  // Error case.
        }
        break;
    }
    return key;
}

void table_remove_white(Table *table) {
    for (int i = 0; i < table->capacity; ++i) {
        Entry *entry = &table->entries[i];
        if (IS_STRING_KEY(entry->key) && !AS_OBJ(key_as_value(entry->key))->is_marked) {
            table_delete(table, entry->key);
        }
    }
}

void mark_table(Table *table) {
    for (int i = 0; i < table->capacity; ++i) {
        Entry *entry = &table->entries[i];
        if (IS_STRING_KEY(entry->key)) {
            mark_object(AS_OBJ(key_as_value(entry->key)));
        }
        mark_value(entry->value);
    }
}
