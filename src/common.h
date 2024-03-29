#ifndef CLOX_COMMON_H
#define CLOX_COMMON_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define NAN_BOXING
#define DEBUG_PRINT_CODE
//#define DEBUG_TRACE_EXECUTION
//#define DEBUG_STRESS_GC
//#define DEBUG_LOG_GC

#define UINT8_COUNT (UINT8_MAX + 1)
#define UINT24_COUNT (UINT8_COUNT * UINT8_COUNT * UINT8_COUNT)
#define UINT24_MAX (UINT24_COUNT - 1)

typedef unsigned long ulong;

#endif
