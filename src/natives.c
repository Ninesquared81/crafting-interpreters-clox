#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "natives.h"
#include "object.h"
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
    {"gets", 0, gets_native},
    {"puts", 1, puts_native},
    {"rand", 0, rand_native},
    {"seedrn", 1, seedrn_native},
};

void init_natives(void) {
    rand_seed = (uint64_t)time(NULL) & LOX_RAND_MAX;
}

static_assert(REAL_NATIVE_COUNT == NATIVE_COUNT);
