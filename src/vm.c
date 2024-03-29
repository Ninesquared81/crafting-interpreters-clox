#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "natives.h"
#include "vm.h"

#define INPUT_MAX 1000

VM vm;

static void reset_stack(void) {
    vm.stack_top = vm.stack;
    vm.frame_count = 0;
    vm.open_upvalues = NULL;
}

static void init_stack(void) {
    vm.stack = ALLOCATE(Value, STACK_SIZE_INIT);
    vm.stack_capacity = STACK_SIZE_INIT;
    reset_stack();
}

static void free_stack(void) {
    FREE_ARRAY(Value, vm.stack, vm.stack_capacity);
    vm.stack = NULL;
    vm.stack_capacity = 0;
    reset_stack();
}

void runtime_error(const char *format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    for (int i = vm.frame_count - 1; i >= 0; --i) {
        CallFrame *frame = &vm.frames[i];
        ObjFunction *function = frame->function;
        size_t instruction = frame->ip - function->chunk.code - 1;
        int line = get_line(&function->chunk.lines, instruction);
        fprintf(stderr, "[line %d] in ", line);
        if (function->name == NULL) {
            fprintf(stderr, "script\n");
        }
        else {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }

    reset_stack();
}

static void define_native(const char *name, ulong arity, NativeFn function) {
    push(OBJ_VAL(copy_string(name, (int)strlen(name))));
    push(OBJ_VAL(new_native(function, arity)));
    table_set(&vm.globals, STRING_KEY(AS_STRING(vm.stack[0])), vm.stack[1]);
    pop();
    pop();
}

void init_vm(void) {
    init_stack();
    vm.objects = NULL;
    vm.bytes_allocated = 0;
    vm.next_gc = 1024 * 1024;

    vm.grey_count = 0;
    vm.grey_capacity = 0;
    vm.grey_stack = NULL;

    init_table(&vm.globals);
    init_table(&vm.strings);
    init_set(&vm.immutable_globals);

    vm.init_string = NULL;
    vm.init_string = copy_string("init", 4);

    init_natives();  // Sets the seed for rand().
    for (int i = 0; i < NATIVE_COUNT; ++i) {
        define_native(natives[i].name, natives[i].arity, natives[i].function);
    }
}

void free_vm(void) {
    free_table(&vm.globals);
    free_table(&vm.strings);
    free_set(&vm.immutable_globals);
    free_stack();

    vm.init_string = NULL;
    free_objects();
}

void push(Value value) {
    // NOTE: Null check removed since init_stack() pre-allocates some stack slots.
    // If vm.stack == NULL, this condition is invalid.
    assert(vm.stack != NULL);
    if (vm.stack_top >= vm.stack + vm.stack_capacity) {
        size_t old_capacity = vm.stack_capacity;
        vm.stack_capacity = GROW_CAPACITY(old_capacity);
        if (IS_OBJ(value)) {
            // Since the stack is dynamically grown, push() can trigger the GC.
            // To avoid the pushed value being collected, we explicitly mark it here.
            AS_OBJ(value)->is_marked = true;
        }
        vm.stack = GROW_ARRAY(Value, vm.stack, old_capacity, vm.stack_capacity);
        vm.stack_top = vm.stack + old_capacity;

        // Make sure slots pointers point into the new allocation.
        // Note: We have to save the offset and use it here, since at this point the slots pointer is invalid.
        for (CallFrame *frame = vm.frames; frame < vm.frames + vm.frame_count; ++frame) {
            frame->slots = vm.stack + frame->slots_offset;

            ObjClosure *closure = frame->closure;
            if (closure == NULL) continue;  // Only update upvalues for a closure.

            for (ulong j = 0; j < closure->upvalue_count; ++j) {
                ObjUpvalue *upvalue = closure->upvalues[j];
                if (upvalue->location != &upvalue->closed) {
                    upvalue->location = vm.stack + upvalue->offset;
                }
            }
        }
    }
    *vm.stack_top++ = value;
}

Value pop(void) {
    return *--vm.stack_top;
}

static Value popn(uint32_t n) {
    vm.stack_top -= n;
    return *vm.stack_top;
}

static Value peek(int distance) {
    return vm.stack_top[-1 - distance];
}

static bool call(ObjFunction *function, ObjClosure *closure, ulong arg_count) {
    if (arg_count != function->arity) {
        runtime_error("Expected %lu arguments but got %lu.", function->arity, arg_count);
        return false;
    }

    if (vm.frame_count == FRAMES_MAX) {
        runtime_error("Stack overflow.");
        return false;
    }

    CallFrame *frame = &vm.frames[vm.frame_count++];
    frame->function = function;
    frame->closure = closure;
    frame->ip = function->chunk.code;
    frame->slots = vm.stack_top - arg_count - 1;
    frame->slots_offset = frame->slots - vm.stack;
    return true;
}

static bool call_native(ObjNative *native, ulong arg_count) {
    if (arg_count != native->arity) {
        runtime_error("Expected %lu arguments but got %lu.", native->arity, arg_count);
        return false;
    }
    Value result;
    if (!native->function(arg_count, vm.stack_top - arg_count, &result)) {
        return false;
    }
    vm.stack_top -= arg_count + 1;
    push(result);
    return true;
}

static bool call_value(Value callee, ulong arg_count) {
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
        case OBJ_BOUND_METHOD: {
            ObjBoundMethod *bound = AS_BOUND_METHOD(callee);
            vm.stack_top[-(long long)arg_count - 1] = bound->receiver;
            return call(bound->method->function, bound->method, arg_count);
        }
        case OBJ_CLASS: {
            ObjClass *class = AS_CLASS(callee);
            vm.stack_top[-(long long)arg_count - 1] = OBJ_VAL(new_instance(class));
            Value initializer;
            if (table_get(&class->methods, STRING_KEY(vm.init_string), &initializer)) {
                ObjClosure *init_closure = AS_CLOSURE(initializer);
                return call(init_closure->function, init_closure, arg_count);
            }
            if (arg_count != 0) {
                runtime_error("Expected 0 arguments but got %lu.", arg_count);
                return false;
            }
            return true;
        }
        case OBJ_CLOSURE: {
            ObjClosure *closure = AS_CLOSURE(callee);
            return call(closure->function, closure, arg_count);
        }
        case OBJ_FUNCTION:
            return call(AS_FUNCTION(callee), NULL, arg_count);
        case OBJ_NATIVE: {
            return call_native(AS_NATIVE(callee), arg_count);
        }
        default:
            break;  // Non-callable object type.
        }
    }
    runtime_error("Can only call functions and classes.");
    return false;
}

static void create_array(ulong length) {
    ObjArray *array = new_array();
    for (Value *value = &vm.stack_top[-(long long)length]; value != vm.stack_top; ++value) {
        write_value_array(&array->elements, *value);
    }
    popn(length);
    push(OBJ_VAL(array));
}

static bool create_dict(ulong length) {
    ObjDict *dict = new_dict();
    dict->length = length;
    for (Value *index = &vm.stack_top[-2 * (long long)length]; index < vm.stack_top; index += 2) {
        Key key = key_from_value(*index);
        if (key.type == KEY_EMPTY) {
            runtime_error("Dict key must be a hashable type.");
            return false;
        }
        Value value = index[1];
        if (!table_set(&dict->contents, key, value)) {
            runtime_error("Dict keys must be unique.");
            return false;
        }
    }
    popn(2 * length);  // Pop all the key-value pairs.
    push(OBJ_VAL(dict));
    return true;
}

static bool invoke_from_class(ObjClass *class, ObjString *name, ulong arg_count) {
    Value method;
    if (!table_get(&class->methods, STRING_KEY(name), &method)) {
        runtime_error("Undefined property '%s'.", name->chars);
        return false;
    }
    ObjClosure *closure = AS_CLOSURE(method);
    return call(closure->function, closure, arg_count);
}

static bool invoke(ObjString *name, ulong arg_count) {
    Value receiver = peek(arg_count);

    if (!IS_INSTANCE(receiver)) {
        runtime_error("Only instances have methods.");
        return false;
    }

    ObjInstance *instance = AS_INSTANCE(receiver);

    Value value;
    if (table_get(&instance->fields, STRING_KEY(name), &value)) {
        vm.stack_top[-(long long)arg_count - 1] = value;
        return call_value(value, arg_count);
    }

    return invoke_from_class(instance->class, name, arg_count);
}

static bool bind_method(ObjClass *class, ObjString *name) {
    Value method;
    if (!table_get(&class->methods, STRING_KEY(name), &method)) {
        runtime_error("Undefined property '%s'.", name->chars);
        return false;
    }

    ObjBoundMethod *bound = new_bound_method(peek(0), AS_CLOSURE(method));
    pop();
    push(OBJ_VAL(bound));
    return true;
}

static ObjUpvalue *capture_upvalue(Value *local) {
    ObjUpvalue *prev_upvalue = NULL;
    ObjUpvalue *upvalue = vm.open_upvalues;
    while (upvalue != NULL && upvalue->location > local) {
        prev_upvalue = upvalue;
        upvalue = upvalue->next;
    }

    if (upvalue != NULL && upvalue->location == local) {
        return upvalue;
    }

    ObjUpvalue *created_upvalue = new_upvalue(local);
    created_upvalue->next = upvalue;

    if (prev_upvalue == NULL) {
        vm.open_upvalues = created_upvalue;
    }
    else {
        prev_upvalue->next = created_upvalue;
    }

    return created_upvalue;
}

static void close_upvalues(Value *last) {
    while (vm.open_upvalues != NULL && vm.open_upvalues->location >= last) {
        ObjUpvalue *upvalue = vm.open_upvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm.open_upvalues = upvalue->next;
    }
}

static void define_method(ObjString *name) {
    Value method = peek(0);
    // TODO: Can probably be reomved soon
    if (!IS_CLOSURE(method)) {
        assert(IS_FUNCTION(method));
        // Wrap method in a closure if it wasn't already wrapped.
        method = OBJ_VAL(new_closure(AS_FUNCTION(method)));
    }
    ObjClass *class = AS_CLASS(peek(1));
    table_set(&class->methods, STRING_KEY(name), method);
    pop();
}

static Value parse_value(const char *string, int length) {
    // Literals.
    switch (length) {
    case 3:
        if (memcmp(string, "nil", 3) == 0) {
            return NIL_VAL;
        }
        break;
    case 4:
        if (memcmp(string, "true", 4) == 0) {
            return BOOL_VAL(true);
        }
        break;
    case 5:
        if (memcmp(string, "false", 5) == 0) {
            return BOOL_VAL(false);
        }
        break;
    }

    // Numbers.
    char *end;
    double number = strtod(string, &end);
    if (end == string + length) {
        // Entire string was a number, so return it.
        return NUMBER_VAL(number);
    }

    // If all the other checks failed, it must just be a string.
    return OBJ_VAL(copy_string(string, length));
}

static bool is_falsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate(void) {
    ObjString *b = AS_STRING(peek(0));
    ObjString *a = AS_STRING(peek(1));
    int length = a->length + b->length;
    char *chars = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    ObjString *result = take_string(chars, length);
    pop();
    pop();
    push(OBJ_VAL(result));
}

static void concatenate_arrays(void) {
    ObjArray *b = AS_ARRAY(peek(0));
    ObjArray *a = AS_ARRAY(peek(1));
    ObjArray *new = copy_array(a);
    push(OBJ_VAL(new));
    extend_value_array(&new->elements, &b->elements);
    pop();  // New array.
    pop();
    pop();
    push(OBJ_VAL(new));
}

static void extend_dict(ObjDict *dict, ObjDict *with) {
    for (ulong i = 0; i < with->length; ++i) {
        Entry *entry = &with->contents.entries[i];
        if (IS_UNOCCUPIED(entry->key)) {
            continue;
        }
        if (table_set(&dict->contents, entry->key, entry->value)) {
            // New key, so update length.
            ++dict->length;
        }
    }
}

static void join_dicts(void) {
    ObjDict *b = AS_DICT(peek(0));
    ObjDict *a = AS_DICT(peek(1));
    ObjDict *new = copy_dict(a);
    push(OBJ_VAL(new));
    extend_dict(new, b);
    pop();  // New array.
    pop();
    pop();
    push(OBJ_VAL(new));
}

static double normalise_index(double index, size_t count) {
    if (index < 0) {
        index += count;
    }
    return index;
}

static bool get_array_index(ObjArray *array, Value index) {
    if (!IS_NUMBER(index)) {
        runtime_error("Index must be a number.");
        return false;
    }
    double real_index = normalise_index(AS_NUMBER(index), array->elements.count);
    if (real_index < 0) {
        runtime_error("Index %g is out of bounds", AS_NUMBER(index));
        return false;
    }
    Value result;
    if (!get_value_array(&array->elements, real_index, &result)) {
        runtime_error("Index %g is out of bounds.", AS_NUMBER(index));
        return false;
    }
    push(result);
    return true;
}

static bool set_array_index(ObjArray *array, Value index, Value value) {
    if (!IS_NUMBER(index)) {
        runtime_error("Index must be a number.");
        return false;
    }
    double real_index = AS_NUMBER(index);
    if (real_index < 0) {
        real_index += array->elements.count;
    }
    if (!set_value_array(&array->elements, real_index, value)) {
        runtime_error("Index %g is out of bounds.", AS_NUMBER(index));
        return false;
    }
    return true;
}

static bool remove_array_index(ObjArray *array, Value index) {
    if (!IS_NUMBER(index)) {
        runtime_error("Index must be a number.");
        return false;
    }
    double real_index = normalise_index(AS_NUMBER(index), array->elements.count);
    if (real_index < 0) {
        runtime_error("Index %g is out of bounds.", AS_NUMBER(index));
        return false;
    }
    Value dummy;
    if (!remove_value_array(&array->elements, (size_t)real_index, &dummy)) {
        runtime_error("Index %g is out of bounds.", AS_NUMBER(index));
        return false;
    }
    return true;
}

static bool get_dict_key(ObjDict *dict, Value key) {
    if (!IS_HASHABLE(key)) {
        runtime_error("Key must be a hashable type.");
        return false;
    }
    Value value;
    if (!table_get(&dict->contents, key_from_value(key), &value)) {
        runtime_error("Unknown key %s.", to_repr_string(key)->chars);
        return false;
    }
    push(value);
    return true;
}

static bool set_dict_key(ObjDict *dict, Value key, Value value) {
    if (!IS_HASHABLE(key)) {
        runtime_error("Key must be a hashable type.");
        return false;
    }
    if (table_set(&dict->contents, key_from_value(key), value)) {
        ++dict->length;
    }
    return true;
}

static bool remove_dict_key(ObjDict *dict, Value key) {
    if (!IS_HASHABLE(key)) {
        runtime_error("Key must be a hashable type.");
        return false;
    }
    if (!table_delete(&dict->contents, key_from_value(key))) {
        runtime_error("Unknown key %s.", to_repr_string(key)->chars);
        return false;
    }
    --dict->length;
    return true;
}

static bool get_string_index(ObjString *string, Value index) {
    if (!IS_NUMBER(index)) {
        runtime_error("Index must be a number.");
        return false;
    }
    double real_index = normalise_index(AS_NUMBER(index), string->length);
    if (real_index < 0 || real_index >= string->length) {
        runtime_error("Index %g is out of bounds.", AS_NUMBER(index));
        return false;
    }
    ObjString *result = copy_string(&string->chars[(size_t)real_index], 1);
    push(OBJ_VAL(result));
    return true;
}

static double normalise_index_saturated(double index, size_t count) {
    index = normalise_index(index, count);
    if (index < 0) {
        index = 0;
    }
    if (index > count) {
        index  = count;
    }
    return index;
}

static bool normalise_slice(size_t count) {
    Value stop = pop();
    Value start = pop();
    if (!IS_NUMBER(start) && !IS_NIL(start)) {
        runtime_error("Slice start must be a number or nil.");
        return false;
    }
    if (!IS_NUMBER(stop) && !IS_NIL(stop)) {
        runtime_error("Slice stop must be a number or nil.");
        return false;
    }
    if (IS_NIL(start)) {
        start = NUMBER_VAL(0.0);
    }
    if (IS_NIL(stop)) {
        stop = NUMBER_VAL((double)count);
    }
    start = NUMBER_VAL(normalise_index_saturated(AS_NUMBER(start), count));
    stop = NUMBER_VAL(normalise_index_saturated(AS_NUMBER(stop), count));
    push(start);
    push(stop);
    return true;
}

static ObjArray *slice_array(ObjArray *array, size_t start, size_t stop) {
    ObjArray *slice = new_array();
    if (stop <= start) return slice;
    
    ValueArray *elements = &slice->elements;
    elements->count = stop - start;
    elements->capacity = elements->count;
    elements->values = ALLOCATE(Value, elements->capacity);

    memcpy(elements->values, &array->elements.values[start],
           elements->count * sizeof(Value));

    return slice;
}

static void set_array_slice(ObjArray *array, size_t start, size_t stop, ObjArray *new) {
    if (start >= array->elements.count) {
        extend_value_array(&array->elements, &new->elements);
        return;
    }
    if (start >= stop) return;

    long long slice_count = stop - start;
    long long diff = (long long)new->elements.count - slice_count;
    shift_value_array(&array->elements, stop, diff);
    memcpy(&array->elements.values[start], new->elements.values,
           new->elements.count * sizeof(Value));
}

static InterpretResult run(void) {
    CallFrame *frame = &vm.frames[vm.frame_count - 1];
    register uint8_t *ip = frame->ip;

#define READ_BYTE() (*ip++)
#define READ_BYTES() (ip += 3, (uint32_t)(      \
                          (ip[-3] << 16) ^      \
                          (ip[-2] << 8)  ^      \
                          (ip[-1]     )))
#define READ_SHORT() (ip += 2, (uint16_t)((ip[-2] << 8) | ip[-1]))
#define READ_CONSTANT() (frame->function->chunk.constants.values[READ_BYTE()])
#define READ_CONSTANT_LONG() (frame->function->chunk.constants.values[READ_BYTES()])
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define READ_STRING_LONG() AS_STRING(READ_CONSTANT_LONG())

#define UPDATE_IP() (frame->ip = ip)
#define RESET_IP() (ip = frame->ip)
#define RUNTIME_ERROR(...) (UPDATE_IP(), runtime_error(__VA_ARGS__))

#define BINARY_OP(value_type, op)                               \
    do {                                                        \
        if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) {       \
            UPDATE_IP();                                        \
            RUNTIME_ERROR("Operands must be numbers.");         \
            return INTERPRET_RUNTIME_ERROR;                     \
        }                                                       \
        double b = AS_NUMBER(pop());                            \
        double a = AS_NUMBER(pop());                            \
        push(value_type(a op b));                               \
    } while (false)

    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        printf("      ");
        for (Value *slot = vm.stack; slot < vm.stack_top; ++slot) {
            printf("[ ");
            print_value(*slot);
            printf(" ]");
        }
        printf("\n");
        disassemble_instruction(&frame->function->chunk, (int)(ip - frame->function->chunk.code));
#endif
        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
        case OP_CONSTANT: {
            Value constant = READ_CONSTANT();
            push(constant);
            break;
        }
        case OP_CONSTANT_LONG: {
            Value constant = READ_CONSTANT_LONG();
            push(constant);
            break;
        }
        case OP_NIL: push(NIL_VAL); break;
        case OP_TRUE: push(BOOL_VAL(true)); break;
        case OP_FALSE: push(BOOL_VAL(false)); break;
        case OP_POP: pop(); break;
        case OP_POPN: popn(READ_BYTE()); break;
        case OP_POPN_LONG: popn(READ_BYTES()); break;
        case OP_GET_GLOBAL: {
            ObjString *name = READ_STRING();
            Value value;
            if (!table_get(&vm.globals, STRING_KEY(name), &value)) {
                RUNTIME_ERROR("Undefined variable '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            push(value);
            break;
        }
        case OP_GET_GLOBAL_LONG: {
            ObjString *name = READ_STRING_LONG();
            Value value;
            if (!table_get(&vm.globals, STRING_KEY(name), &value)) {
                RUNTIME_ERROR("Undefined variable '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            push(value);
            break;
        }
        case OP_GET_LOCAL: {
            uint8_t slot = READ_BYTE();
            push(frame->slots[slot]);
            break;
        }
        case OP_GET_LOCAL_LONG: {
            uint32_t slot = READ_BYTES();
            push(frame->slots[slot]);
            break;
        }
        case OP_DEFINE_GLOBAL: {
            ObjString *name = READ_STRING();
            Key key = STRING_KEY(name);
            table_set(&vm.globals, key, peek(0));
            pop();
            if (READ_BYTE()) {
                set_delete(&vm.immutable_globals, key);
            }
            else {
                set_add(&vm.immutable_globals, key);
            }
            break;
        }
        case OP_DEFINE_GLOBAL_LONG: {
            ObjString *name = READ_STRING_LONG();
            Key key = STRING_KEY(name);
            table_set(&vm.globals, key, peek(0));
            pop();
            if (READ_BYTE()) {
                set_delete(&vm.immutable_globals, key);
            }
            else {
                set_add(&vm.immutable_globals, key);
            }
            break;
        }
        case OP_SET_GLOBAL: {
            ObjString *name = READ_STRING();
            if (set_check(&vm.immutable_globals, STRING_KEY(name))) {
                RUNTIME_ERROR("Cannot assign to a val.");
                return INTERPRET_RUNTIME_ERROR;
            }
            if (table_set(&vm.globals, STRING_KEY(name), peek(0))) {
                table_delete(&vm.globals, STRING_KEY(name));
                RUNTIME_ERROR("Undefined variable '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_SET_GLOBAL_LONG: {
            ObjString *name = READ_STRING_LONG();
            if (set_check(&vm.immutable_globals, STRING_KEY(name))) {
                RUNTIME_ERROR("Cannot assign to a val.");
                return INTERPRET_RUNTIME_ERROR;
            }
            if (table_set(&vm.globals, STRING_KEY(name), peek(0))) {
                table_delete(&vm.globals, STRING_KEY(name));
                RUNTIME_ERROR("Undefined variable '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_SET_LOCAL: {
            uint8_t slot = READ_BYTE();
            frame->slots[slot] = peek(0);
            break;
        }
        case OP_SET_LOCAL_LONG: {
            uint32_t slot = READ_BYTES();
            frame->slots[slot] = peek(0);
            break;
        }
        case OP_GET_UPVALUE: {
            uint8_t slot = READ_BYTE();
            push(*frame->closure->upvalues[slot]->location);
            break;
        }
        case OP_GET_UPVALUE_LONG: {
            uint32_t slot = READ_BYTES();
            push(*frame->closure->upvalues[slot]->location);
            break;
        }
        case OP_SET_UPVALUE: {
            uint8_t slot = READ_BYTE();
            *frame->closure->upvalues[slot]->location = peek(0);
            break;
        }
        case OP_SET_UPVALUE_LONG: {
            uint32_t slot = READ_BYTES();
            *frame->closure->upvalues[slot]->location = peek(0);
            break;
        }
        case OP_GET_PROPERTY: {
            if (!IS_INSTANCE(peek(0))) {
                RUNTIME_ERROR("Only instances have properties.");
                return INTERPRET_RUNTIME_ERROR;
            }

            ObjInstance *instance = AS_INSTANCE(peek(0));
            ObjString *name = READ_STRING();

            Value value;
            if (table_get(&instance->fields, STRING_KEY(name), &value)) {
                pop();  // Instance.
                push(value);
                break;
            }

            UPDATE_IP();
            if (!bind_method(instance->class, name)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_GET_PROPERTY_LONG: {
            if (!IS_INSTANCE(peek(0))) {
                RUNTIME_ERROR("Only instances have properties.");
                return INTERPRET_RUNTIME_ERROR;
            }

            ObjInstance *instance = AS_INSTANCE(peek(0));
            ObjString *name = READ_STRING_LONG();

            Value value;
            if (table_get(&instance->fields, STRING_KEY(name), &value)) {
                pop();  // Instance.
                push(value);
                break;
            }

            UPDATE_IP();
            if (!bind_method(instance->class, name)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_SET_PROPERTY: {
            if (!IS_INSTANCE(peek(1))) {
                RUNTIME_ERROR("Only instances have fields.");
                return INTERPRET_RUNTIME_ERROR;
            }

            ObjInstance *instance = AS_INSTANCE(peek(1));
            table_set(&instance->fields, STRING_KEY(READ_STRING()), peek(0));
            Value value = pop();
            push(value);
            break;
        }
        case OP_SET_PROPERTY_LONG: {
            if (!IS_INSTANCE(peek(1))) {
                RUNTIME_ERROR("Only instances have fields.");
                return INTERPRET_RUNTIME_ERROR;
            }

            ObjInstance *instance = AS_INSTANCE(peek(1));
            table_set(&instance->fields, STRING_KEY(READ_STRING_LONG()), peek(0));
            Value value = pop();
            push(value);
            break;
        }
        case OP_DEL_PROPERTY: {
            if (!IS_INSTANCE(peek(0))) {
                RUNTIME_ERROR("Only instances have fields.");
                return INTERPRET_RUNTIME_ERROR;
            }

            ObjInstance *instance = AS_INSTANCE(peek(0));
            ObjString *name = READ_STRING();
            if (!table_delete(&instance->fields, STRING_KEY(name))) {
                RUNTIME_ERROR("Undefined property '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_DEL_PROPERTY_LONG: {
            if (!IS_INSTANCE(peek(0))) {
                RUNTIME_ERROR("Only instances have fields.");
                return INTERPRET_RUNTIME_ERROR;
            }

            ObjInstance *instance = AS_INSTANCE(peek(0));
            ObjString *name = READ_STRING_LONG();
            if (!table_delete(&instance->fields, STRING_KEY(name))) {
                RUNTIME_ERROR("Undefined property '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_DEL_GLOBAL: {
            ObjString *name = READ_STRING();
            Key key = STRING_KEY(name);
            set_delete(&vm.immutable_globals, key);
            if (!table_delete(&vm.globals, key)) {
                RUNTIME_ERROR("Undefined variable '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_DEL_GLOBAL_LONG: {
            ObjString *name = READ_STRING_LONG();
            Key key = STRING_KEY(name);
            set_delete(&vm.immutable_globals, key);
            if (!table_delete(&vm.globals, key)) {
                RUNTIME_ERROR("Undefined variable '%s'.", name->chars);
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_DEL_INDEX: {
            UPDATE_IP();
            Value index = pop();
            Value iterable = pop();
            if (IS_ARRAY(iterable)) {
                ObjArray *array = AS_ARRAY(iterable);
                if (!remove_array_index(array, index)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
            }
            else if (IS_DICT(iterable)) {
                ObjDict *dict = AS_DICT(iterable);
                if (!remove_dict_key(dict, index)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
            }
            else if (IS_STRING(iterable)) {
                RUNTIME_ERROR("Can't delete a string index.");
                return INTERPRET_RUNTIME_ERROR;
            }
            else {
                RUNTIME_ERROR("Only arrays, dicts and strings can be indexed.");
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_DEL_SLICE: {
            UPDATE_IP();
            Value iterable = peek(2);
            if (IS_ARRAY(iterable)) {
                ObjArray *array = AS_ARRAY(iterable);
                if (!normalise_slice(array->elements.count)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                size_t stop = AS_NUMBER(pop());
                size_t start = AS_NUMBER(pop());
                if (start < stop) {
                    shift_value_array(&array->elements, stop, -(long long)(stop - start));
                }
                pop();  // Array.
            }
            else if (IS_STRING(iterable)) {
                RUNTIME_ERROR("Can't delete a string slice.");
                return INTERPRET_RUNTIME_ERROR;
            }
            else {
                RUNTIME_ERROR("Only arrays and strings can be sliced.");
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_GET_SUPER: {
            ObjString *name = READ_STRING();
            ObjClass *superclass = AS_CLASS(pop());

            UPDATE_IP();
            if (!bind_method(superclass, name)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_GET_SUPER_LONG: {
            ObjString *name = READ_STRING_LONG();
            ObjClass *superclass = AS_CLASS(pop());

            UPDATE_IP();
            if (!bind_method(superclass, name)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_EQUAL: {
            Value b = pop();
            Value a = pop();
            push(BOOL_VAL(values_equal(a, b)));
            break;
        }
        case OP_GREATER:
            BINARY_OP(BOOL_VAL, >);
            break;
        case OP_LESS:
            BINARY_OP(BOOL_VAL, <);
            break;
        case OP_ADD: {
            if (IS_STRING(peek(0)) || IS_STRING(peek(1))) {
                ObjString *a = to_string(peek(0));
                ObjString *b = to_string(peek(1));
                pop();
                pop();
                push(OBJ_VAL((Obj *)b));
                push(OBJ_VAL((Obj *)a));
                concatenate();
            }
            else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
                double b = AS_NUMBER(pop());
                double a = AS_NUMBER(pop());
                push(NUMBER_VAL(a + b));
            }
            else if (IS_ARRAY(peek(0)) && IS_ARRAY(peek(1))) {
                concatenate_arrays();
            }
            else if (IS_DICT(peek(0)) && IS_DICT(peek(1))) {
                join_dicts();
            }
            else {
                RUNTIME_ERROR("Operands must be two numbers, two arrays, two dicts,"
                              " or any value and a string.");
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_SUBTRACT:
            BINARY_OP(NUMBER_VAL, -);
            break;
        case OP_MULTIPLY:
            BINARY_OP(NUMBER_VAL, *);
            break;
        case OP_DIVIDE:
            BINARY_OP(NUMBER_VAL, /);
            break;
        case OP_NOT:
            push(BOOL_VAL(is_falsey(pop())));
            break;
        case OP_NEGATE:
            if (!IS_NUMBER(peek(0))) {
                RUNTIME_ERROR("Operand must be a number.");
                return INTERPRET_RUNTIME_ERROR;
            }
            push(NUMBER_VAL(-AS_NUMBER(pop())));
            break;
        case OP_INPUT: {
            char buf[INPUT_MAX];
            fflush(stdout);
            fgets(buf, INPUT_MAX, stdin);
            if (ferror(stdin)) {
                RUNTIME_ERROR("Error reading from stdin.");
                return INTERPRET_RUNTIME_ERROR;
            }
            if (feof(stdin)) {
                // EOF: send empty string.
                clearerr(stdin);  // Clear the EOF status.
                push(OBJ_VAL(FROM_STRING_LITERAL("")));
            }
            else {
                int length = strlen(buf);
                if (length > 0 && buf[length - 1] == '\n') {
                    // Decrease length and replace '\n' with null byte.
                    buf[--length] = '\0';
                }
                push(parse_value(buf, length));
            }
            break;
        }
        case OP_PRINT: {
            // TODO: This could be changed to a call to `to_string()` since we already defined it.
            // i.e.
            // printf("%s\n", to_string(pop()));
            print_value(pop());
            printf("\n");
            break;
        }
        case OP_JUMP: {
            uint16_t offset = READ_SHORT();
            ip += offset;
            break;
        }
        case OP_JUMP_IF_FALSE: {
            uint16_t offset = READ_SHORT();
            if (is_falsey(peek(0))) ip += offset;
            break;
        }
        case OP_LOOP: {
            uint16_t offset = READ_SHORT();
            ip -= offset;
            break;
        }
        case OP_ARRAY: {
            ulong length = READ_BYTE();
            create_array(length);
            break;
        }
        case OP_ARRAY_LONG: {
            ulong length = READ_BYTES();
            create_array(length);
            break;
        }
        case OP_DICT: {
            ulong length = READ_BYTE();
            UPDATE_IP();
            if (!create_dict(length)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_DICT_LONG: {
            ulong length = READ_BYTES();
            UPDATE_IP();
            if (!create_dict(length)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_GET_INDEX: {
            Value index = pop();
            Value iterable = pop();
            UPDATE_IP();
            if (IS_ARRAY(iterable)) {
                ObjArray *array = AS_ARRAY(iterable);
                if (!get_array_index(array, index)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
            }
            else if (IS_STRING(iterable)) {
                ObjString *string = AS_STRING(iterable);
                if (!get_string_index(string, index)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
            }
            else if (IS_DICT(iterable)) {
                ObjDict *dict = AS_DICT(iterable);
                if (!get_dict_key(dict, index)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
            }
            else {
                RUNTIME_ERROR("Only arrays, dicts and strings can be indexed.");
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_SET_INDEX: {
            Value value = pop();  // The value to be set.
            Value index = pop();
            Value iterable = peek(0);  // Not popped.
            UPDATE_IP();
            if (IS_ARRAY(iterable)) {
                ObjArray *array = AS_ARRAY(iterable);
                if (!set_array_index(array, index, value)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
            }
            else if (IS_STRING(iterable)) {
                RUNTIME_ERROR("Can't set a string index.");
            }
            else if (IS_DICT(iterable)) {
                ObjDict *dict = AS_DICT(iterable);
                if (!set_dict_key(dict, index, value)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
            }
            else {
                RUNTIME_ERROR("Only arrays, dicts and strings can be indexed.");
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_GET_SLICE: {
            UPDATE_IP();
            Value iterable = peek(2);
            if (IS_ARRAY(iterable)) {
                if (!normalise_slice(AS_ARRAY(iterable)->elements.count)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                double stop = AS_NUMBER(pop());
                double start = AS_NUMBER(pop());
                ObjArray *array = AS_ARRAY(peek(0));
                ObjArray *slice = slice_array(array, (size_t)start, (size_t)stop);
                pop();  // Array.
                push(OBJ_VAL(slice));
            }
            else if (IS_STRING(iterable)) {
                if (!normalise_slice(AS_STRING(iterable)->length)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                size_t stop = AS_NUMBER(pop());
                size_t start = AS_NUMBER(pop());
                ObjString *string = AS_STRING(peek(0));
                Value result = OBJ_VAL(
                    copy_string(&string->chars[start],
                                (start < stop) ? stop - start : 0));
                pop();  // String.
                push(result);
            }
            else {
                RUNTIME_ERROR("Only arrays and strings can be sliced.");
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_SET_SLICE: {
            Value value = pop();  // The value to set.
            if (!IS_ARRAY(value) && !IS_STRING(value)) {
                RUNTIME_ERROR("Can only set slice with another array or string.");
            }
            UPDATE_IP();
            Value iterable = peek(2);  // Left on stack to be used in expressions.
            if (IS_ARRAY(iterable)) {
                ObjArray *array = AS_ARRAY(iterable);
                normalise_slice(array->elements.count);
                double stop = AS_NUMBER(pop());
                double start = AS_NUMBER(pop());
                ObjArray *slice;
                if (IS_ARRAY(value)) {
                    slice = AS_ARRAY(value);
                }
                else if (IS_STRING(value)) {
                    slice = new_array();
                    ObjString *string = AS_STRING(value);
                    for (int i = 0; i < string->length; ++i) {
                        write_value_array(&slice->elements,
                                          OBJ_VAL(copy_string(&string->chars[i], 1)));
                    }
                }
                else {
                    RUNTIME_ERROR("Invalid program state [%s line %s]",
                                  __FILE__, __LINE__);
                    return INTERPRET_RUNTIME_ERROR;
                }
                set_array_slice(array, (size_t)start, (size_t)stop, slice);
            }
            else if (IS_STRING(iterable)) {
                RUNTIME_ERROR("Can't set a string slice.");
                return INTERPRET_RUNTIME_ERROR;
            }
            else {
                RUNTIME_ERROR("Only arrays and strings can be sliced.");
                return INTERPRET_RUNTIME_ERROR;
            }
            break;
        }
        case OP_CALL: {
            ulong arg_count = READ_BYTE();
            UPDATE_IP();
            if (!call_value(peek(arg_count), arg_count)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            frame = &vm.frames[vm.frame_count - 1];
            RESET_IP();
            break;
        }
        case OP_CALL_LONG: {
            ulong arg_count = READ_BYTES();
            UPDATE_IP();
            if (!call_value(peek(arg_count), arg_count)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            frame = &vm.frames[vm.frame_count - 1];
            RESET_IP();
            break;
        }
        case OP_INVOKE: {
            ObjString *method = READ_STRING();
            ulong arg_count = READ_BYTE();
            if (!invoke(method, arg_count)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            UPDATE_IP();
            frame = &vm.frames[vm.frame_count - 1];
            RESET_IP();
            break;
        }
        case OP_INVOKE_LONG: {
            ObjString *method = READ_STRING_LONG();
            ulong arg_count = READ_BYTES();
            if (!invoke(method, arg_count)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            UPDATE_IP();
            frame = &vm.frames[vm.frame_count - 1];
            RESET_IP();
            break;
        }
        case OP_SUPER_INVOKE: {
            ObjString *method = READ_STRING();
            ulong arg_count = READ_BYTE();
            ObjClass *superclass = AS_CLASS(pop());
            UPDATE_IP();
            if (!invoke_from_class(superclass, method, arg_count)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            frame = &vm.frames[vm.frame_count - 1];
            RESET_IP();
            break;
        }
        case OP_SUPER_INVOKE_LONG: {
            ObjString *method = READ_STRING_LONG();
            ulong arg_count = READ_BYTES();
            ObjClass *superclass = AS_CLASS(pop());
            UPDATE_IP();
            if (!invoke_from_class(superclass, method, arg_count)) {
                return INTERPRET_RUNTIME_ERROR;
            }
            frame = &vm.frames[vm.frame_count - 1];
            RESET_IP();
            break;
        }
        case OP_CLOSURE: {
            ObjFunction *function = AS_FUNCTION(READ_CONSTANT());
            ObjClosure *closure = new_closure(function);
            push(OBJ_VAL(closure));
            for (ulong i = 0; i < closure->upvalue_count; ++i) {
                uint8_t first_byte = READ_BYTE();
                bool is_local = first_byte & 1;
                bool is_long = first_byte >> 7;
                uint32_t index = (!is_long) ? READ_BYTE() : READ_BYTES();
                if (is_local) {
                    closure->upvalues[i] = capture_upvalue(frame->slots + index);
                }
                else {
                    closure->upvalues[i] = frame->closure->upvalues[index];
                }
            }
            break;
        }
        case OP_CLOSURE_LONG: {
            ObjFunction *function = AS_FUNCTION(READ_CONSTANT_LONG());
            ObjClosure *closure = new_closure(function);
            push(OBJ_VAL(closure));
            for (ulong i = 0; i < closure->upvalue_count; ++i) {
                uint8_t first_byte = READ_BYTE();
                bool is_local = first_byte & 1;
                bool is_long = first_byte >> 7;
                uint32_t index = (!is_long) ? READ_BYTE() : READ_BYTES();
                if (is_local) {
                    closure->upvalues[i] = capture_upvalue(frame->slots + index);
                }
                else {
                    closure->upvalues[i] = frame->closure->upvalues[index];
                }
            }
            break;
        }
        case OP_CLOSE_UPVALUE:
            close_upvalues(vm.stack_top - 1);
            pop();
            break;
        case OP_RETURN: {
            Value result = pop();
            close_upvalues(frame->slots);
            vm.frame_count--;
            if (vm.frame_count == 0) {
                pop();
                return INTERPRET_OK;
            }

            vm.stack_top = frame->slots;
            push(result);
            frame = &vm.frames[vm.frame_count - 1];
            RESET_IP();
            break;
        }
        case OP_CLASS: {
            push(OBJ_VAL(new_class(READ_STRING())));
            break;
        }
        case OP_CLASS_LONG: {
            push(OBJ_VAL(new_class(READ_STRING_LONG())));
            break;
        }
        case OP_INHERIT: {
            Value superclass = peek(1);
            if (!IS_CLASS(superclass)) {
                RUNTIME_ERROR("Superclass must be a class.");
                return INTERPRET_RUNTIME_ERROR;
            }

            ObjClass *subclass = AS_CLASS(peek(0));
            table_add_all(&AS_CLASS(superclass)->methods, &subclass->methods);
            pop();  // Subclass.
            break;
        }
        case OP_METHOD:
            define_method(READ_STRING());
            break;
        case OP_METHOD_LONG:
            define_method(READ_STRING_LONG());
            break;
        }
    }

#undef READ_BYTE
#undef READ_BYTES
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_CONSTANT_LONG
#undef READ_STRING
#undef UPDATE_IP
#undef RUNTIME_ERROR
#undef BINARY_OP
}

InterpretResult interpret(const char *source) {
    ObjFunction *function = compile(source);
    if (function == NULL) return INTERPRET_COMPILE_ERROR;

    push(OBJ_VAL(function));
    call(function, NULL, 0);

    return run();
}
