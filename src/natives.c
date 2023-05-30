#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "natives.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define REAL_NATIVE_COUNT (sizeof natives / sizeof natives[0])

static bool clock_native(ulong arg_count, Value *args, Value *result) {
    (void)arg_count; (void)args;
    *result = NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
    return true;
}

static uint64_t rand_seed;
static uint64_t lox_rand(void) {
#define LCG_m ((uint64_t)LOX_RAND_MAX)      // Large power of 2.
#define LCG_a ((uint64_t)(UINT24_COUNT + 1))  // (a - 1) div. by all prime factors of m, (a - 1) div. by 4 (since m is).
#define LCG_c ((uint64_t)(3343 * 729))      // m and c are relatively prime (prime fact. 3^3 * 3343)
    
    // Use a linear congruential generator to get random numbers.
    rand_seed = (LCG_a * rand_seed + LCG_c) % LCG_m;
    return rand_seed;
#undef LCG_m
#undef LCG_a
#undef LCG_c
}

static bool rand_native(ulong arg_count, Value *args, Value *result) {
    (void)arg_count; (void)args;
    *result = NUMBER_VAL((double)lox_rand() / (LOX_RAND_MAX + 1));
    return true;
}

static bool seedrn_native(ulong arg_count, Value *args, Value *result) {
    (void)arg_count;
    Value seed = args[0];
    if (!IS_NUMBER(seed)) {
        runtime_error("Argument to seedrn() must be a number.");
        return false;
    }
    rand_seed = (uint64_t)AS_NUMBER(seed);
    *result = NIL_VAL;
    return true;
}

static bool round_native(ulong arg_count, Value *args, Value *result) {
    (void)arg_count;
    Value number = args[0];
    if (!IS_NUMBER(number)) {
        runtime_error("Argument to round() must be a number.");
        return false;
    }
    *result = NUMBER_VAL(round(AS_NUMBER(number)));
    return true;
}

static bool has_property_native(ulong arg_count, Value *args, Value *result) {
    (void)arg_count;
    if (!IS_INSTANCE(args[0])) {
        runtime_error("First argument to has_property() must be an instance.");
        return false;
    }
    ObjInstance *instance = AS_INSTANCE(args[0]);
    if (!IS_STRING(args[1])) {
        runtime_error("Second argument to has_property() must be a string.");
        return false;
    }
    ObjString *name = AS_STRING(args[1]);
    Value dummy;  // Dummy Value to pass to table_get().
    bool has_property = table_get(&instance->fields, STRING_KEY(name), &dummy);
    *result = BOOL_VAL(has_property);
    return true;
}

static bool remove_property_native(ulong arg_count, Value *args, Value *result) {
    (void)arg_count;
    if (!IS_INSTANCE(args[0])) {
        runtime_error("First argument to remove_property() must be an instance.");
        return false;
    }
    ObjInstance *instance = AS_INSTANCE(args[0]);
    if (!IS_STRING(args[1])) {
        runtime_error("Second argument to remove_property() must be a string");
        return false;
    }
    ObjString *name = AS_STRING(args[1]);
    if (!table_delete(&instance->fields, STRING_KEY(name))) {
        runtime_error("Undefined property '%s'.", name->chars);
        return false;
    }
    *result = NIL_VAL;
    return true;
}

static bool get_property_native(ulong arg_count, Value *args, Value *result) {
    (void)arg_count;
    if (!IS_INSTANCE(args[0])) {
        runtime_error("First argument to get_property() must be an instance.");
        return false;
    }
    ObjInstance *instance = AS_INSTANCE(args[0]);
    if (!IS_STRING(args[1])) {
        runtime_error("Second argument to get_property() must be a string");
        return false;
    }
    ObjString *name = AS_STRING(args[1]);
    if (!table_get(&instance->fields, STRING_KEY(name), result)) {
        runtime_error("Undefined property '%s'.", name->chars);
        return false;
    }
    return true;
}

static bool set_property_native(ulong arg_count, Value *args, Value *result) {
    (void)arg_count;
    if (!IS_INSTANCE(args[0])) {
        runtime_error("First argument to set_property() must be an instance.");
        return false;
    }
    ObjInstance *instance = AS_INSTANCE(args[0]);
    if (!IS_STRING(args[1])) {
        runtime_error("Second argument to set_property() must be a string");
        return false;
    }
    ObjString *name = AS_STRING(args[1]);
    table_set(&instance->fields, STRING_KEY(name), args[2]);
    *result = NIL_VAL;
    return true;
}

static bool gets_native(ulong arg_count, Value *args, Value *result) {
    (void)arg_count; (void)args;
    char buf[GETS_MAX];
    if (fgets(buf, GETS_MAX, stdin) == NULL) {
        runtime_error("Error reading from stdin.");
        return false;
    }
    int length = strlen(buf);
    if (buf[length - 1] == '\n') {
        // Trim off trailing newline if present.
        --length;
    }
    *result = OBJ_VAL(copy_string(buf, length));
    return true;
}

static bool puts_native(ulong arg_count, Value *args, Value *result) {
    (void)arg_count;
    Value string = args[0];
    if (!IS_STRING(string)) {
        runtime_error("Argument to puts() must be a string.");
        return false;
    }
    puts(AS_CSTRING(string));
    *result = NIL_VAL;
    return true;
}

NativeEntry natives[] = {
    {"clock", 0, clock_native},
    {"get_property", 2, get_property_native},
    {"gets", 0, gets_native},
    {"has_property", 2, has_property_native},
    {"puts", 1, puts_native},
    {"rand", 0, rand_native},
    {"remove_property", 2, remove_property_native},
    {"round", 1, round_native},
    {"seedrn", 1, seedrn_native},
    {"set_property", 3, set_property_native},
};

void init_natives(void) {
    rand_seed = (uint64_t)time(NULL) & LOX_RAND_MAX;
}

static_assert(REAL_NATIVE_COUNT == NATIVE_COUNT);
