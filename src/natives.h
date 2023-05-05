#ifndef CLOX_NATIVES_H
#define CLOX_NATIVES_H

#include "object.h"

#define NATIVE_COUNT 6

#define LOX_RAND_MAX (((uint64_t)UINT32_MAX + 1) * ((uint64_t)UINT16_MAX + 1))  // 2^48
#define GETS_MAX 1000
#define GETS_LEN (GETS_MAX - 1)

typedef struct {
    const char *name;
    ulong arity;
    NativeFn function;
} NativeEntry;

extern NativeEntry natives[/*NATIVE_COUNT*/];

void init_natives(void);

#endif
