#include <assert.h>
#include <time.h>

#include "common.h"
#include "natives.h"
#include "value.h"
#include "vm.h"


static bool clock_native(ulong arg_count, Value *args, Value *result) {
    (void)arg_count; (void)args;
    *result = NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
    return true;
}

static uint64_t rand_seed;
static uint64_t lox_rand(void) {
#define LCG_m ((uint64_t)LOX_RAND_MAX)      // Large power of 2.
#define LCG_a ((uint64_t)(UINT8_COUNT + 1))  // (a - 1) div. by all prime factors of m, (a - 1) div. by 4 (since m is).
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

NativeEntry natives[] = {
    {"clock", 0, clock_native},
    {"rand", 0, rand_native},
};

void init_natives(void) {
    rand_seed = (uint64_t)time(NULL) & LOX_RAND_MAX;
}

static_assert(sizeof natives / sizeof *natives == NATIVE_COUNT);
