#ifndef CLOX_VALUE_H
#define CLOX_VALUE_H

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

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

#define HASH_BOOL(value)  ((value).as.boolean ? 1389110 : 25689038)
#define HASH_NIL          30477
#define HASH_NUMBER(value) hash_double((value).as.number)

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
