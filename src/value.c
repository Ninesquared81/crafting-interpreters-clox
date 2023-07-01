#include <assert.h>
#include <math.h>

#include <stdio.h>
#include <string.h>

#include "object.h"
#include "memory.h"
#include "value.h"

void init_value_array(ValueArray *array) {
    array->values = NULL;
    array->capacity = 0;
    array->count = 0;
}

void copy_value_array(ValueArray *from, ValueArray *to) {
    to->count = from->count;
    to->capacity = to->count;
    to->values = ALLOCATE(Value, to->capacity);
    memcpy(to->values, from->values, to->count * sizeof(Value));
}

void shift_value_array(ValueArray *array, size_t from, long long by) {
    if (by > 0) {
        if (array->capacity < array->count + by) {
            size_t old_capacity = array->capacity;
            array->capacity = array->count + by;
            array->values = GROW_ARRAY(Value, array->values, old_capacity, array->capacity);
        }
        for (size_t i = array->count; i >= from; --i) {
            array->values[i + by] = array->values[i];
        }
    }
    else {
        for (size_t i = from; i < array->count; ++i) {
            array->values[i + by] = array->values[i];
        }
    }
    array->count += by;
}

void write_value_array(ValueArray *array, Value value) {
    if (array->capacity < array->count + 1) {
        size_t old_capacity = array->capacity;
        array->capacity = GROW_CAPACITY(old_capacity);
        array->values = GROW_ARRAY(Value, array->values, old_capacity,  array->capacity);
    }

    array->values[array->count++] = value;
}

bool insert_value_array(ValueArray *array, size_t index, Value value) {
    if (index >= array->count) {
        return false;
    }
    
    if (array->capacity < array->count + 1) {
        size_t old_capacity = array->capacity;
        array->capacity = GROW_CAPACITY(old_capacity);
        array->values = GROW_ARRAY(Value, array->values, old_capacity,  array->capacity);
    }

    for (Value *vp = &array->values[array->count]; vp >= array->values + index; --vp) {
        vp[1] = *vp;
    }
    array->values[index] = value;
    ++array->count;
    return true;
}

bool remove_value_array(ValueArray *array, size_t index, Value *value) {
    if (index >= array->count) {
        return false;
    }

    *value = array->values[index];
    for (Value *vp = &array->values[index]; vp < &array->values[array->count]; ++vp) {
        *vp = vp[1];
    }
    --array->count;
    return true;
}

bool get_value_array(const ValueArray *array, size_t index, Value *value) {
    if (index >= array->count) {
        return false;
    }

    *value = array->values[index];
    return true;
}

bool set_value_array(ValueArray *array, size_t index, Value value) {
    if (index >= array->count) {
        return false;
    }

    array->values[index] = value;
    return true;
}

void extend_value_array(ValueArray *array, const ValueArray *with) {
    size_t new_count = array->count + with->count;
    if (array->capacity < new_count) {
        size_t old_capacity = array->capacity;
        array->capacity = new_count;
        array->values = GROW_ARRAY(Value, array->values, old_capacity, array->capacity);
    }
    memcpy(&array->values[array->count], with->values, with->count * sizeof(Value));
    array->count = new_count;
}

ValueArray cut_value_array(ValueArray *array, size_t index) {
    ValueArray tail;

    if (index >= array->count) {
        init_value_array(&tail);
        return tail;
    }
    
    tail.count = array->count - index;
    tail.capacity = tail.count;
    tail.values = ALLOCATE(Value, tail.capacity);
    memcpy(tail.values, &array->values[index], tail.count * sizeof(Value));
    array->count = index;

    return tail;
}

void free_value_array(ValueArray *array) {
    FREE_ARRAY(Value, array->values, array->capacity);
    init_value_array(array);
}

void print_value(Value value) {
    printf("%s", to_string(value)->chars);
}

static bool arrays_equal(ValueArray *a, ValueArray *b) {
    if (a->count != b->count) return false;
    for (size_t i = 0; i < a->count; ++i) {
        Value value_a, value_b;
        get_value_array(a, i, &value_a);
        get_value_array(b, i, &value_b);
        if (!values_equal(value_a, value_b)) {
            return false;
        }
    }
    return true;
}

bool values_equal(Value a, Value b) {
    if (a.type != b.type) return false;
    switch (a.type) {
    case VAL_BOOL:   return AS_BOOL(a) ==  AS_BOOL(b);
    case VAL_NIL:    return true;
    case VAL_NUMBER: return AS_NUMBER(a) == AS_NUMBER(b);
    case VAL_OBJ:
        if (AS_OBJ(a)->type == OBJ_ARRAY && AS_OBJ(b)->type == OBJ_ARRAY) {
            return arrays_equal(&AS_ARRAY(a)->elements, &AS_ARRAY(b)->elements);
        }
        else {
            return AS_OBJ(a) == AS_OBJ(b);
        }
    default:         return false;  // Unreachable.
    }
}

uint32_t hash_double(double number) {
    static_assert(sizeof number == 8);
    if (number == 0 || isnan(number)) return 0;
    if (isinf(number)) return UINT32_MAX;
    union {double x; uint64_t n;} pun;
    pun.x = number;
    return pun.n ^ (pun.n >> 32);
}
