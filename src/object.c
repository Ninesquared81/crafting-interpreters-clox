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

ObjClosure *new_closure(ObjFunction *function) {
    ObjClosure *closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
    closure->function = function;
    return closure;
}

ObjFunction *new_function(void) {
    ObjFunction *function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
    function->arity = 0;
    function->name = NULL;
    init_chunk(&function->chunk);
    return function;
}

ObjNative *new_native(NativeFn function, ulong arity) {
    ObjNative *native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
    native->function = function;
    native->arity = arity;
    return native;
}

static ObjString *allocate_string(char *chars, int length, uint32_t hash) {
    ObjString *string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;
    table_set(&vm.strings, STRING_KEY(string), NIL_VAL);
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
        switch (AS_OBJ(value)->type) {
        case OBJ_STRING:
            return AS_STRING(value);
        case OBJ_CLOSURE: {
            ObjString *name = AS_CLOSURE(value)->function->name;
            if (name == NULL) return FROM_STRING_LITERAL("<script>");
            return name;
        }
        case OBJ_FUNCTION: {
            ObjString *name = AS_FUNCTION(value)->name;
            if (name == NULL) return FROM_STRING_LITERAL("<script>");
            return name;
        }
        case OBJ_NATIVE:
            return FROM_STRING_LITERAL("<native fn>");
        default:
            return FROM_STRING_LITERAL("<Unknown object>");  // Unreachable.
        }
    default:
        return FROM_STRING_LITERAL("<Unknown value>");  // Unreachable.
    }
}

static void print_function(ObjFunction *function) {
    if (function->name == NULL) {
        printf("<script>");
        return;
    }
    printf("<fn %s>", function->name->chars);
}

void print_object(Value value) {
    switch (OBJ_TYPE(value)) {
    case OBJ_CLOSURE:
        print_function(AS_CLOSURE(value)->function);
        break;
    case OBJ_FUNCTION:
        print_function(AS_FUNCTION(value));
        break;
    case OBJ_NATIVE:
        printf("<native fn>");
        break;
    case OBJ_STRING:
        printf("%s", AS_CSTRING(value));
        break;
    }
}
