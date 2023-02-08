#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, object_type) \
    (type *)allocate_object(sizeof(type), object_type)

#define FROM_STRING_LITERAL(string_literal) \
    copy_string(string_literal, sizeof string_literal - 1)

static Obj *allocate_object(size_t size, ObjType type) {
    Obj *object = (Obj *)reallocate(NULL, 0, size);
    object->type = type;

    object->next = vm.objects;
    vm.objects = object;
    return object;
}

static ObjString *allocate_string(char *chars, int length, uint32_t hash) {
    ObjString *string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;
    table_set(&vm.strings, string, NIL_VAL);
    return string;
}

static uint32_t hash_string(const char *key, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; i++) {
        hash ^= (uint8_t)key[i];
        hash *= 16777619;
    }
    return hash;
}

ObjString *take_string(char *chars, int length) {
    uint32_t hash = hash_string(chars, length);
    ObjString *interned = table_find_string(&vm.strings, chars, length, hash);

    if (interned != NULL) {
        FREE_ARRAY(char, chars, length + 1);
        return interned;
    }

    return allocate_string(chars, length, hash);
}

ObjString *copy_string(const char *chars, int length) {
    uint32_t hash = hash_string(chars, length);
    ObjString *interned = table_find_string(&vm.strings, chars, length, hash);

    if (interned != NULL) return interned;

    char *heap_chars = ALLOCATE(char, length + 1);
    memcpy(heap_chars, chars, length);
    heap_chars[length] = '\0';
    return allocate_string(heap_chars, length, hash);
}

ObjString *to_string(Value value) {
    switch (value.type) {
    case VAL_BOOL:
        return (value.as.boolean) ?
            FROM_STRING_LITERAL("true") : FROM_STRING_LITERAL("false");
    case VAL_NIL:
        return FROM_STRING_LITERAL("nil");
    case VAL_NUMBER: {
        size_t size = snprintf(NULL, 0, "%g", value.as.number) + 1;
        char *chars = (char *)reallocate(NULL, 0, size);
        snprintf(chars, size, "%g", value.as.number);
        return take_string(chars, size - 1);
    }
    case VAL_OBJ:
        switch (value.as.obj->type) {
        case OBJ_STRING:
            return (ObjString *)value.as.obj;
        default:
            return FROM_STRING_LITERAL("<Unknown object>");  // Unreachable.
        }
    default:
        return FROM_STRING_LITERAL("<Unknown value>");  // Unreachable.
    }
}

void print_object(Value value) {
    switch (OBJ_TYPE(value)) {
    case OBJ_STRING:
        printf("%s", AS_CSTRING(value));
        break;
    }
}