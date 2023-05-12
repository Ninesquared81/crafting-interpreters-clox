#ifndef CLOX_OBJECT_H
#define CLOX_OBJECT_H

#include "common.h"
#include "chunk.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_CLOSURE(vale) is_obj_type(value, OBJ_CLOSURE)
#define IS_FUNCTION(value) is_obj_type(value, OBJ_FUNCTION)
#define IS_NATIVE(value) is_obj_type(value, OBJ_NATIVE)
#define IS_STRING(value) is_obj_type(value, OBJ_STRING)

#define AS_CLOSURE(value) ((ObjClosure *)AS_OBJ(value))
#define AS_FUNCTION(value) ((ObjFunction *)AS_OBJ(value))
//#define AS_NATIVE(value) (((ObjNative *)AS_OBJ(value))->function)
#define AS_NATIVE(value) ((ObjNative *)AS_OBJ(value))
#define AS_STRING(value) ((ObjString *)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString *)AS_OBJ(value))->chars)

typedef enum {
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_NATIVE,
    OBJ_STRING,
    OBJ_UPVALUE,
} ObjType;

struct Obj {
    ObjType type;
    struct Obj *next;
};

typedef struct {
    Obj obj;
    ulong arity;
    ulong upvalue_count;
    Chunk chunk;
    ObjString *name;
    struct ObjClosure *closure;
} ObjFunction;

typedef bool (*NativeFn)(ulong arg_count, Value *args, Value *result);

typedef struct {
    Obj obj;
    ulong arity;
    NativeFn function;
} ObjNative;

struct ObjString {
    Obj obj;
    int length;
    char *chars;
    uint32_t hash;
};

typedef struct ObjUpvalue {
    Obj obj;
    Value *location;
    ptrdiff_t offset;
    Value closed;
    struct ObjUpvalue *next;
} ObjUpvalue;

typedef struct ObjClosure {
    Obj obj;
    ObjFunction *function;
    ObjUpvalue **upvalues;
    ulong upvalue_count;
} ObjClosure;

ObjClosure *new_closure(ObjFunction *function);
ObjFunction *new_function(void);
ObjNative *new_native(NativeFn function, ulong arity);
ObjString *take_string(char *chars, int length);
ObjString *copy_string(const char *chars, int length);
ObjString *to_string(Value value);
ObjUpvalue *new_upvalue(Value *slot);
void print_object(Value value);

static inline bool is_obj_type(Value value, ObjType type) {
    return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif
