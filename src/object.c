#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, object_type) \
    (type *)allocate_object(sizeof(type), object_type)


const char *const obj_type_names[] = {
    [OBJ_ARRAY] = "OBJ_ARRAY",
    [OBJ_BOUND_METHOD] = "OBJ_BOUND_METHOD",
    [OBJ_CLASS] = "OBJ_CLASS",
    [OBJ_CLOSURE] = "OBJ_CLOSURE",
    [OBJ_DICT] = "OBJ_DICT",
    [OBJ_FUNCTION] = "OBJ_FUNCTION",
    [OBJ_INSTANCE] = "OBJ_INSTANCE",
    [OBJ_NATIVE] = "OBJ_NATIVE",
    [OBJ_STRING] = "OBJ_STRING",
    [OBJ_UPVALUE] = "OBJ_UPVALUE",
};

#define ARRAY_COUNT 10
static_assert(ARRAY_COUNT == OBJ_TYPE_COUNT);
#undef ARRAY_COUNT

static Obj *allocate_object(size_t size, ObjType type) {
    Obj *object = (Obj *)reallocate(NULL, 0, size);
    object->type = type;
    object->is_marked = false;

    object->next = vm.objects;
    vm.objects = object;

#ifdef DEBUG_LOG_GC
    printf("%p allocate %zu for type %d (%s)\n",
           (void*)object, size, object->type, obj_type_names[object->type]);
#endif

    return object;
}

ObjArray *new_array(void) {
    ObjArray *array = ALLOCATE_OBJ(ObjArray, OBJ_ARRAY);
    init_value_array(&array->elements);
    return array;
}

ObjArray *copy_array(ObjArray *from) {
    ObjArray *copy = new_array();
    copy_value_array(&from->elements, &copy->elements);
    return copy;
}

ObjBoundMethod *new_bound_method(Value receiver, ObjClosure *method) {
    ObjBoundMethod *bound = ALLOCATE_OBJ(ObjBoundMethod, OBJ_BOUND_METHOD);
    bound->receiver = receiver;
    bound->method = method;
    return bound;
}

ObjClass *new_class(ObjString *name) {
    ObjClass *class = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);
    class->name = name;
    init_table(&class->methods);
    return class;
}

ObjClosure *new_closure(ObjFunction *function) {
    ObjUpvalue **upvalues = ALLOCATE(ObjUpvalue*, function->upvalue_count);
    for (ulong i = 0; i < function->upvalue_count; ++i) {
        upvalues[i] = NULL;
    }

    ObjClosure *closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
    closure->function = function;
    closure->upvalues = upvalues;
    closure->upvalue_count = function->upvalue_count;
    return closure;
}

ObjDict *new_dict(void) {
    ObjDict *dict = ALLOCATE_OBJ(ObjDict, OBJ_DICT);
    init_table(&dict->contents);
    dict->length = 0;
    return dict;
}

ObjDict *copy_dict(ObjDict *from) {
    ObjDict *dict = new_dict();
    table_add_all(&from->contents, &dict->contents);
    dict->length = from->length;
    return dict;
}

ObjFunction *new_function(void) {
    ObjFunction *function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
    function->arity = 0;
    function->upvalue_count = 0;
    function->name = NULL;
    init_chunk(&function->chunk);
    return function;
}

ObjInstance *new_instance(ObjClass *class) {
    ObjInstance *instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
    instance->class = class;
    init_table(&instance->fields);
    return instance;
}

ObjNative *new_native(NativeFn function, ulong arity) {
    ObjNative *native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
    native->function = function;
    native->arity = arity;
    return native;
}

static ObjString *allocate_string(char *chars, int length, uint32_t hash) {
    ObjString *string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
    string->length = length;
    string->chars = chars;
    string->hash = hash;

    push(OBJ_VAL(string));
    table_set(&vm.strings, STRING_KEY(string), NIL_VAL);
    pop();

    return string;
}

static uint32_t hash_string(const char *key, int length) {
    uint32_t hash = 2166136261u;
    for (int i = 0; i < length; i++) {
        hash ^= (uint8_t)key[i];
        hash *= 16777619;
    }
    return hash;
}

ObjString *take_string(char *chars, int length) {
    uint32_t hash = hash_string(chars, length);
    ObjString *interned = table_find_string(&vm.strings, chars, length, hash);

    if (interned != NULL) {
        FREE_ARRAY(char, chars, length + 1);
        return interned;
    }

    return allocate_string(chars, length, hash);
}

ObjString *copy_string(const char *chars, int length) {
    uint32_t hash = hash_string(chars, length);
    ObjString *interned = table_find_string(&vm.strings, chars, length, hash);

    if (interned != NULL) return interned;

    char *heap_chars = ALLOCATE(char, length + 1);
    memcpy(heap_chars, chars, length);
    heap_chars[length] = '\0';
    return allocate_string(heap_chars, length, hash);
}

ObjString *to_repr_string(Value value) {
    if (IS_STRING(value)) {
        ObjString *string = AS_STRING(value);
        char *heap_chars = ALLOCATE(char, string->length + 2 + 1);
        heap_chars[0] = '"';
        memcpy(&heap_chars[1], string->chars, string->length);
        int new_length = string->length + 2;
        heap_chars[new_length - 1] = '"';
        heap_chars[new_length] = '\0';
        return take_string(heap_chars, new_length);
    }
    else {
        return to_string(value);
    }
}

static ObjString *array_to_string(ObjArray *array) {
    int length = 0;
    size_t capacity = 2 + 1;  // 2 for '[' and ']'; +1 for '\0'.
    char *chars = ALLOCATE(char, capacity);
    chars[length++] = '[';
    ValueArray *elements = &array->elements;
    if (array->elements.count != 0) {
        ObjString *element = to_repr_string(elements->values[0]);
        size_t old_capacity = capacity;
        capacity += element->length;
        chars = GROW_ARRAY(char, chars, old_capacity, capacity);
        memcpy(&chars[length], element->chars, element->length);
        length += element->length;
        for (size_t i = 1; i < elements->count; ++i) {
            element = to_repr_string(elements->values[i]);
            old_capacity = capacity;
            capacity += 2 + element->length;  // +2 for ", ".
            chars = GROW_ARRAY(char, chars, old_capacity, capacity);
            chars[length++] = ',';
            chars[length++] = ' ';
            memcpy(&chars[length], element->chars, element->length);
            length += element->length;
        }
    }
    chars[length++] = ']';
    chars[length] = '\0';
    return take_string(chars, length);
}

static ObjString *dict_to_string(ObjDict *dict) {
    int length = 0;
    size_t capacity = 2 + 1;
    char *heap_chars = ALLOCATE(char, capacity);
    heap_chars[length++] = '{';
    if (dict->length != 0) {
        Entry *entry = dict->contents.entries;
        while (IS_UNOCCUPIED(entry->key)) {
            ++entry;
        }
        ObjString *key = to_repr_string(key_as_value(entry->key));
        ObjString *value = to_repr_string(entry->value);
        size_t old_capacity = capacity;
        capacity += key->length + 2 + value->length;
        heap_chars = GROW_ARRAY(char, heap_chars, old_capacity, capacity);
        memcpy(&heap_chars[length], key->chars, key->length);
        length += key->length;
        heap_chars[length++] = ':';
        heap_chars[length++] = ' ';
        memcpy(&heap_chars[length], value->chars, value->length);
        length += value->length;
        for (ulong i = 1; i < dict->length; ++i) {
            do {
                ++entry;
            } while (IS_UNOCCUPIED(entry->key));
            key = to_repr_string(key_as_value(entry->key));
            value = to_repr_string(entry->value);
            old_capacity = capacity;
            capacity += 2 + key->length + 2 + value->length;
            heap_chars = GROW_ARRAY(char, heap_chars, old_capacity, capacity);
            heap_chars[length++] = ',';
            heap_chars[length++] = ' ';
            memcpy(&heap_chars[length], key->chars, key->length);
            length += key->length;
            heap_chars[length++] = ':';
            heap_chars[length++] = ' ';
            memcpy(&heap_chars[length], value->chars, value->length);
            length += value->length;
        }
    }
    heap_chars[length++] = '}';
    heap_chars[length] = '\0';
    return take_string(heap_chars, length);
}

static ObjString *obj_to_string(Obj *object) {
    switch (object->type) {
    case OBJ_STRING:
        return (ObjString *)object;
    case OBJ_ARRAY:
        return array_to_string((ObjArray *)object);
    case OBJ_CLOSURE: {
        ObjString *name = ((ObjClosure *)object)->function->name;
        if (name == NULL) return FROM_STRING_LITERAL("<script>");
        return name;
    }
    case OBJ_DICT:
        return dict_to_string((ObjDict *)object);
    case OBJ_FUNCTION: {
        ObjString *name = ((ObjFunction *)object)->name;
        if (name == NULL) return FROM_STRING_LITERAL("<script>");
        return name;
    }
    case OBJ_NATIVE:
        return FROM_STRING_LITERAL("<native fn>");
    default:
        return FROM_STRING_LITERAL("<Unknown object>");  // Unreachable.
    }
}

static ObjString *number_to_string(double number) {
    size_t size = snprintf(NULL, 0, "%g", number) + 1;
    char *chars = (char *)reallocate(NULL, 0, size);
    snprintf(chars, size, "%g", number);
    return take_string(chars, size - 1);
}

ObjString *to_string(Value value) {
#ifdef NAN_BOXING
    if (IS_BOOL(value)) {
        return (AS_BOOL(value)) ?
            /* NOTE: The repeated macros are required because they use sizeof */
            FROM_STRING_LITERAL("true") : FROM_STRING_LITERAL("false");
    }
    else if (IS_NIL(value)) {
        return FROM_STRING_LITERAL("nil");
    }
    else if (IS_NUMBER(value)) {
        return number_to_string(AS_NUMBER(value));
    }
    else if (IS_OBJ(value)) {
        return obj_to_string(AS_OBJ(value));
    }
    else {
        return FROM_STRING_LITERAL("<Unknown value>");
    }
#else
    switch (value.type) {
    case VAL_BOOL:
        return (value.as.boolean) ?
            FROM_STRING_LITERAL("true") : FROM_STRING_LITERAL("false");
    case VAL_NIL:
        return FROM_STRING_LITERAL("nil");
    case VAL_NUMBER:
        return number_to_string(AS_NUMBER(value));
    case VAL_OBJ:
        return obj_to_string(AS_OBJ(value));
    default:
        return FROM_STRING_LITERAL("<Unknown value>");  // Unreachable.
    }
#endif
}

ObjUpvalue *new_upvalue(Value *slot) {
    ObjUpvalue *upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
    upvalue->location = slot;
    upvalue->offset = slot - vm.stack;
    upvalue->closed = NIL_VAL;
    upvalue->next = NULL;
    return upvalue;
}

/*
static void print_array(ObjArray *array) {
    printf("[");
    size_t count = array->elements.count;
    Value *values = array->elements.values;
    if (count >= 1) {
        print_value(values[0]);
    }
    for (size_t i = 1; i < count; ++i) {
        printf(", ");
        print_value(values[i]);
    }
    printf("]");
}


static void print_function(ObjFunction *function) {
    if (function->name == NULL) {
        printf("<script>");
        return;
    }
    printf("<fn %s>", function->name->chars);
}

void print_object(Value value) {
    switch (OBJ_TYPE(value)) {
    case OBJ_ARRAY:
        print_array(AS_ARRAY(value));
        break;
    case OBJ_BOUND_METHOD:
        print_function(AS_BOUND_METHOD(value)->method->function);
        break;
    case OBJ_CLASS:
        printf("%s", AS_CLASS(value)->name->chars);
        break;
    case OBJ_CLOSURE:
        print_function(AS_CLOSURE(value)->function);
        break;
    case OBJ_INSTANCE:
        printf("%s instance", AS_INSTANCE(value)->class->name->chars);
        break;
    case OBJ_FUNCTION:
        print_function(AS_FUNCTION(value));
        break;
    case OBJ_NATIVE:
        printf("<native fn>");
        break;
    case OBJ_STRING:
        printf("%s", AS_CSTRING(value));
        break;
    case OBJ_UPVALUE:
        printf("upvalue");
        break;
    default:
        break;
    }
}
*/
