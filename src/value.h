#ifndef CLOX_VALUE_H
#define CLOX_VALUE_H

#include <string.h>

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

#ifdef NAN_BOXING

#define SIGN_BIT ((uint64_t)0x8000000000000000)
#define QNAN     ((uint64_t)0x7ffc000000000000)

#define TAG_NIL   1  // 01.
#define TAG_FALSE 2  // 10.
#define TAG_TRUE  3  // 11.

typedef uint64_t Value;

#define IS_BOOL(value)   (((value) | 1) == TRUE_VAL)
#define IS_NIL(value)    ((value) == NIL_VAL)
#define IS_NUMBER(value) (((value) & QNAN) != QNAN)
#define IS_OBJ(value) \
    (((value) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

#define AS_BOOL(value)   ((value) == TRUE_VAL)
#define AS_NUMBER(value) value_to_num(value)
#define AS_OBJ(value) \
    ((Obj *)(uintptr_t)((value) & ~(SIGN_BIT | QNAN)))

#define BOOL_VAL(b)     ((b) ? TRUE_VAL : FALSE_VAL)
#define FALSE_VAL       ((Value)(uint64_t)(QNAN | TAG_FALSE))
#define TRUE_VAL        ((Value)(uint64_t)(QNAN | TAG_TRUE))
#define NIL_VAL         ((Value)(uint64_t)(QNAN | TAG_NIL))
#define NUMBER_VAL(num) num_to_value(num)
#define OBJ_VAL(obj) \
    (Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))

static inline double value_to_num(Value value) {
    double num;
    memcpy(&num, &value, sizeof(Value));
    return num;
}

static inline Value num_to_value(double num) {
    Value value;
    memcpy(&value, &num, sizeof(double));
    return value;
}

#else

typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
    VAL_OBJ,
} ValueType;

typedef struct {
    ValueType type;
    union {
        bool boolean;
        double number;
        Obj *obj;
    } as;
} Value;

#define IS_BOOL(value)    ((value).type == VAL_BOOL)
#define IS_NIL(value)     ((value).type == VAL_NIL)
#define IS_NUMBER(value)  ((value).type == VAL_NUMBER)
#define IS_OBJ(value)     ((value).type == VAL_OBJ)

#define AS_OBJ(value)     ((value).as.obj)
#define AS_BOOL(value)    ((value).as.boolean)
#define AS_NUMBER(value)  ((value).as.number)

#define BOOL_VAL(value)   ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL           ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object)   ((Value){VAL_OBJ, {.obj = (Obj *)object}})

#endif

#define HASH_BOOL(value)  (AS_BOOL(value) ? 1389110 : 25689038)
#define HASH_NIL          30477
#define HASH_NUMBER(value) hash_double(AS_NUMBER(value))

typedef struct {
    size_t capacity;
    size_t count;
    Value *values;
} ValueArray;

bool values_equal(Value a, Value b);
void init_value_array(ValueArray *array);
void copy_value_array(ValueArray *from, ValueArray *to);
void shift_value_array(ValueArray *array, size_t from, long long by);
void write_value_array(ValueArray *array, Value value);
bool insert_value_array(ValueArray *array, size_t index, Value value);
bool remove_value_array(ValueArray *array, size_t index, Value *value);
bool get_value_array(const ValueArray *array, size_t index, Value *value);
bool set_value_array(ValueArray *array, size_t index, Value value);
void extend_value_array(ValueArray *array, const ValueArray *with);
ValueArray cut_value_array(ValueArray *array, size_t index);
void free_value_array(ValueArray *array);
void print_value(Value value);

uint32_t hash_double(double number);

#endif
