#include <assert.h>
#include <cerrno>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libcup.h"

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/mman.h>
#include <unistd.h>
#endif

#ifndef PAGESIZE
#define PAGESIZE 4096
#endif

#define FILE_NOT_FOUND -2
#define FILE_NOT_RECOGNIZED -3

#define CUP_ERROR_FORMAT 3
#define CUP_ERROR_MEM 2

static inline uint64_t align_up(uint64_t value, uint64_t alignment) {
  return (value + alignment - 1) & ~(alignment - 1);
}
static inline uint64_t align_down(uint64_t value, uint64_t alignment) {
  return value & ~(alignment - 1);
}

static void default_error_func(CUPErrorLevel level, const char *msg) {
  switch (level) {
  case CEL_NONE:
    return;
  case CEL_INFO:
    fputs("INFO: ", stderr);
    break;
  case CEL_WARN:
    fputs("WARN: ", stderr);
    break;
  case CEL_ERRO:
    fputs("ERROR: ", stderr);
    break;
  }
  fputs(msg, stderr);
  fputc('\n', stderr);
}
static void *default_reallocator(void *ptr, size_t size) {
  if (size != 0) {
    void *p = realloc(ptr, size);
    if (p)
      return p;
    default_error_func(CEL_ERRO, "memory full for reallocator");
    exit(CUP_ERROR_MEM);
  }
  free(ptr);
  return nullptr;
}

static void *calloc(size_t size, CUPReallocFunc *reallocator) {
  void *ptr = reallocator(nullptr, size);
  memset(ptr, 0, size);
  return ptr;
}
#define callocT(T, reallocator) (T *)calloc(sizeof(T), reallocator)

#if 1

typedef struct {
  size_t size;
  size_t capacity;
  void *data;
} CVector;

static void vector_push(CVector *left, void *data, size_t size,
                        CUPReallocFunc *reallocator) {
  if (size == 0 || data == nullptr)
    return;
  size_t newsize = left->size + size;
  if (left->capacity < newsize) {
    do {
      left->capacity = left->capacity ? left->capacity * 2 : 8;
    } while (left->capacity < newsize);
    left->data = reallocator(left->data, left->capacity);
  }
  memcpy((char *)left->data + left->size, data, size);
  left->size += size;
}

static void push_vector(CVector *left, CVector right,
                        CUPReallocFunc *reallocator) {
  vector_push(left, right.data, right.size, reallocator);
  if (right.data)
    reallocator(right.data, 0);
}

static void vector_fpush(CVector *vector, void *data, size_t size,
                         CUPReallocFunc *reallocator) {
  CVector new_vector = {};
  vector_push(&new_vector, data, size, reallocator);
  push_vector(&new_vector, *vector, reallocator);
  memcpy(vector, &new_vector, sizeof(CVector));
}

static void fpush_vector(CVector *left, CVector right,
                         CUPReallocFunc *reallocator) {
  vector_fpush(left, right.data, right.size, reallocator);
  if (right.data)
    reallocator(right.data, 0);
}

#define vector_push_node(left, data, r)                                        \
  vector_push((left), (data), sizeof(Node), r)
#define vector_fpush_node(left, data, r)                                       \
  vector_fpush((left), (data), sizeof(Node), r)
#define vector_push_uint(left, data)                                           \
  vector_push((left), (data), sizeof(uint8_t), r)
#define vector_push_page(left, data)                                           \
  vector_push((left), (data), sizeof(Page), r)

static CVector vempty = {0, 0, nullptr};

#endif // 1

#if 1

#include "./tokens.c"

typedef struct {
  CTType type;
  uint16_t col;
  uint32_t line;
} Loc;

typedef union {
  CTType type;
  Loc loc;
} Token;

typedef struct CTypeList {
  struct CTypeList *next;
  CType type;
} CTypeList;

enum { // 255
  R_VOID,
  R_FUNCTION,
  R_NUMBER,
  R_DOUBLE,
  R_POINTER,
  R_ARRAY,
  R_COMPLEX,
};

static size_t cup_type_size(const CType *type) {
  if (type == nullptr)
    return 0;
  switch (type->type) {
  default:
    return 0;
  case R_FUNCTION:
    return sizeof(void *);
  case R_NUMBER:
    return sizeof(size_t);
  case R_DOUBLE:
    return sizeof(double);
  case R_POINTER:
    return sizeof(void *);
  case R_ARRAY:
    return type->as.array.count * cup_type_size(type->as.array.ptrtype);
  case R_COMPLEX: {
    size_t r = 0;
    for (uint8_t i = 0; i < type->as.complex.count; i++) {
      r += cup_type_size(type->as.complex.types[i]);
    }
    return r;
  }
  }
}

static const char *CType_name(const CType *type) {
  if (type == nullptr)
    return "UNKNOWN";
  switch (type->type) {
  case R_VOID:
    return "VOID";
  case R_FUNCTION:
    return "FUNCTION";
  case R_NUMBER:
    return "NUMBER";
  case R_DOUBLE:
    return "DOUBLE";
  case R_POINTER:
    return "POINTER";
  case R_ARRAY:
    return "ARRAY";
  case R_COMPLEX:
    return "COMPLEX";
  default:
    return "UNKNOWN";
  }
}

typedef struct {
  const CType *type;
  size_t name;
  size_t value;
} CVariable;

typedef union {
  Token token;
  CTType type;
  Loc loc;
  size_t value;
  double dvalue;
} Node;

typedef struct {
  Node *nodes;
  size_t count;
  size_t pos;
} Nodes_;

typedef struct {
  size_t *var;
  size_t ptr;
  size_t flags;
} CRealloc;

typedef struct {
  uint8_t *data;
  size_t size;
  size_t capacity;
  CVector reallocs;
  CUPState *state;
} CBuffer;

static void *allocMemory(size_t size) {
#if defined(_WIN32) || defined(_WIN64)
  return VirtualAlloc(NULL, size, MEM_COMMIT | MEM_RESERVE,
                      PAGE_EXECUTE_READWRITE);
#else
  void *mem = mmap(NULL, size, PROT_READ | PROT_WRITE | PROT_EXEC,
                   MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  return (mem == MAP_FAILED) ? NULL : mem;
#endif
}
static void freeMemory(void *ptr, size_t size) {
#if defined(_WIN32) || defined(_WIN64)
  (void)size;
  VirtualFree(ptr, 0, MEM_RELEASE);
#else
  munmap(ptr, size);
#endif
}

typedef struct Page {
  void *data;
  size_t size;
} Page;

struct CUPState {
  void *(*reallocator)(void *, size_t);
  void (*error_func)(CUPErrorLevel, const char *);

  const char *input_stream;
  union {
    CVariable *variable;
    Token token;
    CTType type;
    Loc loc;
    struct {
      Node node;
      union {
        size_t value;
        Node node2;
      };
    };
  };
  uint8_t *names;

  uint8_t *data;
  size_t data_size;

  CTypeList *types;

  CVector vars;
  CVector pages;

  size_t ssss;
};

static_assert(sizeof(CUPState) == 128);

static char *cup_sprintf(CUPReallocFunc *reallocator, const char *format, ...) {
  va_list args, args_copy;
  int len;
  size_t size = 64;
  void *buffer = reallocator(nullptr, size);
  va_start(args, format);
  while (1) {
    va_copy(args_copy, args);
    len = vsnprintf((char *)buffer, size, format, args_copy);
    va_end(args_copy);
    if (len < 0) { // Encoding error
      va_end(args);
      reallocator(buffer, 0);
      default_error_func(CEL_ERRO, "format error");
      return nullptr;
    }
    if ((size_t)len < size) {
      va_end(args);
      return (char *)buffer;
    }
    size = len;
    buffer = reallocator(buffer, ++size);
  }
}

#define cup_errorf(state, f, ...)                                              \
  do {                                                                         \
    char *str = cup_sprintf((state)->reallocator, (f), __VA_ARGS__);           \
    (state)->error_func(CEL_ERRO, str);                                        \
    (state)->reallocator(str, 0);                                              \
  } while (0)
#define cup_error(state, msg) cup_errorf(state, "%s", msg)
#endif // 1

static void emit_error(CBuffer *buf, size_t offset) {
  if (buf == nullptr)
    return;
  if (buf->data == nullptr) {
    buf->data = (uint8_t *)buf->state->reallocator(nullptr, PAGESIZE);
    buf->capacity = PAGESIZE;
  }
  if ((buf->size + offset) > buf->capacity) {
    buf->capacity += PAGESIZE;
    buf->data = (uint8_t *)buf->state->reallocator(buf->data, buf->capacity);
  }
}

static void emit(CBuffer *buf, void *value, size_t size) {
  if (buf == nullptr)
    return;
  emit_error(buf, size);
  memcpy((uint8_t *)buf->data + buf->size, value, size);
  buf->size += size;
}
#define emit8(buf, value)                                                      \
  do {                                                                         \
    uint8_t vbuf = value;                                                      \
    emit(buf, &vbuf, sizeof(uint8_t));                                         \
  } while (0)
#define emit16(buf, value)                                                     \
  do {                                                                         \
    uint16_t vbuf = value;                                                     \
    emit(buf, &vbuf, sizeof(uint16_t));                                        \
  } while (0)
#define emit32(buf, value)                                                     \
  do {                                                                         \
    uint32_t vbuf = value;                                                     \
    emit(buf, &vbuf, sizeof(uint32_t));                                        \
  } while (0)
#define emit64(buf, value)                                                     \
  do {                                                                         \
    uint64_t vbuf = value;                                                     \
    emit(buf, &vbuf, sizeof(uint64_t));                                        \
  } while (0)

static const CType *cup_type_get(CUPState *state, const CType type) {
  for (CTypeList *it = state->types; it; it = it->next) {
    const CType *t = &it->type;
    if (t->type != type.type)
      continue;
    if (memcmp(t, &type, sizeof(type)) != 0)
      continue;
    return t;
  }
  CTypeList *item = callocT(CTypeList, state->reallocator);
  memcpy(&item->type, &type, sizeof(type));
  item->next = state->types;
  state->types = item;
  return &item->type;
}

static size_t strindex(const char *str, const char c) {
  const char *ptr = str;
  while (*ptr != c)
    ptr++;
  return ptr - str;
}

static inline bool isID(const unsigned char c) {
  return ((c > '/') && (c < ':' || c > '@') && (c < '[' || c > '^') &&
          (c < '{' || c > 0x7F));
}

void nextChar(const char c, Loc *loc) {
  loc->col += (c != '\r');
  if (c == '\n') {
    loc->line++;
    loc->col = 1;
  }
}

bool zeroChar(CUPState *state) {
  const char *p = state->input_stream + 1;
  if ((*p | 32) == 'b') {
    for (++p; *p == '0' || *p == '1' || *p == '_'; ++p)
      if (*p != '_')
        state->value = (state->value << 1) | (*p & 1);
    state->node.loc.col += (p - state->input_stream);
    state->input_stream = p;
    return true;
  }
  if ((*p | 32) == 'o') {
    for (++p; (*p >= '0' && *p <= '7') || *p == '_'; ++p)
      if (*p != '_')
        state->value = (state->value << 3) | (*p & 7);
    state->node.loc.col += (p - state->input_stream);
    state->input_stream = p;
    return true;
  }
  if ((*p | 32) == 'x') {
    for (++p; (*p >= '0' && *p <= '9') || (*p >= 'a' && *p <= 'f') ||
              (*p >= 'A' && *p <= 'F') || *p == '_';
         ++p) {
      size_t c_int = (*p >= 'a' && *p <= 'f')   ? (*p - 87)
                     : (*p >= 'A' && *p <= 'F') ? (*p - 55)
                                                : (*p - '0');
      if (*p != '_')
        state->value = (state->value << 4) | c_int;
    }
    state->node.loc.col += (p - state->input_stream);
    state->input_stream = p;
    return true;
  }
  return false;
}

static const char *getString(CUPState *state, size_t index) {
  if (state->names == nullptr || index == SIZE_MAX) {
    return "";
  }

  // Find the start sentinel [0, 0]
  uint8_t *ptr = state->names - 1;
  while (ptr > state->names - 1000) { // safety limit
    if (ptr[0] == '\0' && ptr[-1] == '\0') {
      // Found [0, 0] sentinel
      ptr++; // Move to position after [0, 0]
      break;
    }
    ptr--;
  }

  // Now walk forward and count strings
  size_t current_index = 0;
  while (ptr < state->names) {
    if (*ptr == '\0') {
      ptr++;
      continue;
    }
    if (current_index == index)
      return (const char *)ptr;
    while (*ptr++)
      ;
    current_index++;
  }

  return nullptr;
}

static uint8_t *idToken(CUPState *state, const char *str, size_t len) {
  uint8_t *current = state->names - 1;
  state->value = 0;
  uint8_t *keyword = nullptr;
  for (size_t l = 0;; current--) {
    if (current[-1] != '\0') {
      // Found [non-0, X] continue to found [0, non-0](strings_start)
      l++;
      continue;
    }
    if (*current == '\0') {
      // Found [0, 0] sentinel so keyword not found
      current--;
      if (keyword == nullptr) {
        const size_t size = state->names - current;
        state->names = (uint8_t *)state->reallocator(current, size + len + 1);
        state->names += size;
        keyword = state->names;
        memcpy(state->names, str, len);
        state->names[len] = '\0';
        state->names += len + 1;
      }
      return keyword;
    }
    state->value++;
    if (keyword == nullptr) {
      // Found [0, non-0] strings_start
      if (l == len && memcmp(str, current, l) == 0) {
        // Match found and we need go to start
        keyword = current;
        state->value = 0;
      } else // No match, continue to next keyword in backward order
        l = 0;
    }
  }
  // Unrachable
}

static void getToken(CUPState *state) {
  static const CTType types1[] = {T_ADD, T_SUB,  T_MUL,   T_DIV, T_MOD, T_AND,
                                  T_OR,  T_LESS, T_GREAT, T_XOR, T_NOT, T_EQ};
  static const CTType doubles[] = {T_ADDEQ,   T_SUBEQ, T_MULEQ, T_DIVEQ,
                                   T_MODEQ,   T_ANDEQ, T_OREQ,  T_LESSEQ,
                                   T_GREATEQ, T_XOREQ, T_NOTEQ, T_EQEQ};
  static const CTType types2[] = {
      T_ORB,   T_CRB,    T_OCB,    T_CCB, T_OSB, T_CSB,  T_DOT,   T_COMMA,
      T_COLON, T_SCOLON, T_IMPORT, T_ASK, T_AT,  T_THIS, T_CATNL, T_EXTERNAL};

  state->value = 0;
  while (*state->input_stream == ' ' || *state->input_stream == '\t')
    nextChar(*state->input_stream++, &state->loc);

  Loc start_loc = state->loc;
  static const char *temp = nullptr;
  char input_char = *state->input_stream;

  switch (input_char) {
  case '\0':
    state->type = T_EOF;
    return;
  case '\n':
  case '\r':
    nextChar(*state->input_stream++, &state->loc);
    state->type = T_NL;
    return;
  case '"': {
    for (temp = ++state->input_stream; *state->input_stream != '\0';
         nextChar(*state->input_stream++, &state->loc)) {
      if (*state->input_stream == '"') {
        nextChar(*state->input_stream++, &state->loc);
        break;
      }
    }
    size_t len = (state->input_stream - temp) - 1;
    state->loc = start_loc;

    if (state->data_size > len) {
      const size_t size = state->data_size - len;
      for (size_t i = 0; i < size; i++) {
        if (memcmp(state->data + i, temp, len) == 0) {
          state->value = i;
          state->type = T_STRING;
          return;
        }
      }
    }

    state->data =
        (uint8_t *)state->reallocator(state->data, state->data_size + len);
    memcpy(state->data + state->data_size, temp, len);
    state->data_size += len;
    state->data[state->data_size++] = '\0';
    state->type = T_STRING;
    return;
  }
  case '0':
    if (zeroChar(state)) {
      state->loc = start_loc;
      state->type = T_NUMBER;
      return;
    }
    [[fallthrough]];
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
    while ((*state->input_stream >= '0' && *state->input_stream <= '9') ||
           *state->input_stream == '_') {
      if (*state->input_stream != '_')
        state->value = state->value * 10 + (*state->input_stream - '0');
      nextChar(*state->input_stream++, &state->loc);
    }
    state->loc = start_loc;
    state->type = T_NUMBER;
    return;
  case '+':
  case '-':
  case '*':
  case '/':
  case '%':
  case '&':
  case '|':
  case '<':
  case '>':
  case '^':
  case '!':
  case '=':
    input_char = strindex("+-*/%&|<>^!=", input_char);
    nextChar(*state->input_stream++, &state->loc);
    if (*state->input_stream != '=') {
      state->loc = start_loc;
      state->type = types1[(uint8_t)input_char];
      return;
    }
    nextChar(*state->input_stream++, &state->loc);
    state->loc = start_loc;
    state->type = doubles[(uint8_t)input_char];
    return;
  case '.': {
    nextChar(*state->input_stream++, &state->loc);
    if (*state->input_stream == '.') {
      if (state->input_stream[1] == '.') {
        nextChar(*state->input_stream++, &state->loc);
        nextChar(*state->input_stream++, &state->loc);
        state->loc = start_loc;
        state->type = T_VARG;
        return;
      }
    }
    state->loc = start_loc;
    state->type = T_DOT;
    return;
  }
  case '(':
  case ')':
  case '{':
  case '}':
  case '[':
  case ']':
  case ',':
  case ':':
  case ';':
  case '#':
  case '?':
  case '@':
  case '$':
  case '\\':
  case '~':
    input_char = strindex("(){}[].,:;#?@$\\~", input_char);
    nextChar(*state->input_stream++, &state->loc);
    state->loc = start_loc;
    state->type = types2[(uint8_t)input_char];
    return;
  default: {
    temp = state->input_stream;

    do {
      nextChar(*state->input_stream++, &state->loc);
    } while (isID(*state->input_stream));
    size_t len = state->input_stream - temp;
    state->loc = start_loc;

    if (len == 2 && memcmp(temp, "if", 2) == 0) {
      state->type = T_IF;
      return;
    }
    if (len == 3 && memcmp(temp, "for", 3) == 0) {
      state->type = T_FOR;
      return;
    }
    if (len == 4 && memcmp(temp, "else", 4) == 0) {
      state->type = T_ELSE;
      return;
    }
    if (len == 5 && memcmp(temp, "break", 5) == 0) {
      state->type = T_BREAK;
      return;
    }
    if (len == 5 && memcmp(temp, "while", 5) == 0) {
      state->type = T_WHILE;
      return;
    }
    if (len == 6 && memcmp(temp, "return", 6) == 0) {
      state->type = T_RETURN;
      return;
    }
    if (len == 8 && memcmp(temp, "continue", 8) == 0) {
      state->type = T_CONTINUE;
      return;
    }

    idToken(state, temp, len);
    state->type = T_IDENTIFIER;
    return;
  }
  }
}

static void skipSpaces(CUPState *state) {
  while (*state->input_stream == ' ' || *state->input_stream == '\t' ||
         *state->input_stream == '\n' || *state->input_stream == '\r')
    nextChar(*state->input_stream++, &state->loc);
  getToken(state);
}

#define expectAndNext(ntype)                                                   \
  do {                                                                         \
    if (state->type != ntype) {                                                \
      cup_errorf(state, "Expected " #ntype ", but got " NODEFMT,               \
                 NODEFMTV(state->node));                                       \
      exit(1);                                                                 \
    }                                                                          \
    getToken(state);                                                           \
  } while (0)

static size_t getScopeVar(CUPState *state, size_t name) {
  if (name == SIZE_MAX)
    return SIZE_MAX;
  for (size_t count = state->vars.size / sizeof(CVariable); count--;) {
    if (((CVariable *)state->vars.data)[count].name == name)
      return count;
  }
  return SIZE_MAX;
}

static size_t addScopeVar(CUPState *state, size_t name, const CType *type,
                          size_t value) {
  CVariable v = {type, name, value};
  size_t i = state->vars.size / sizeof(v);
  vector_push(&state->vars, &v, sizeof(v), state->reallocator);
  return i;
}

const CType *defType(CUPState *state, const Node *nodes, size_t *pos,
                     const CType *value) {
  if (nodes == nullptr)
    return nullptr;
  assert(pos != nullptr);
  if (pos == nullptr) {
    cup_error(state, "defType: pos is null");
    return nullptr;
  }
  Node n = nodes[(*pos)++];
  const CType *left, *right;
  switch (n.type) {
  default:
    cup_errorf(state, "defType UNKNOWN node make for " NODEFMT, NODEFMTV(n));
    return nullptr;
  case N_ADD:
  case N_SUB:
  case N_MUL:
  case N_DIV:
  case N_MOD: {
    left = defType(state, nodes, pos, nullptr);
    right = defType(state, nodes, pos, nullptr);

    if (value)
      break;
    if (left == right)
      return left;
    cup_error(state, "getType left and right are not compariable");
    return right;
  }
  case N_ASSIGN:
  case N_ADDASSIGN:
  case N_SUBASSIGN:
  case N_MULASSIGN:
  case N_DIVASSIGN: {
    right = defType(state, nodes, pos, nullptr);
    return defType(state, nodes, pos, right);
  }
  case N_CALL: {
    const CType *func_type = defType(state, nodes, pos, nullptr);
    const CType *args = defType(state, nodes, pos, nullptr);
    cup_errorf(state, "defType CALL %s %s", CType_name(func_type),
               CType_name(args));
    if (func_type && func_type->type == R_FUNCTION) {
      return func_type->as.func.return_type;
    }
    cup_error(state, "getType call not Function");
    return nullptr;
  }
  case N_COMMA: {
    size_t count = nodes[(*pos)++].value;
    if (count > UINT8_MAX)
      break;
    const CType *types[UINT8_MAX] = {};
    for (size_t i = 0; i < count; i++)
      types[i] = defType(state, nodes, pos, nullptr);
    return cup_type_get_complex(state, count, types);
  }
  case N_NUMBER: {
    (*pos)++;
    if (value)
      break;
    return cup_type_get_number(state, sizeof(size_t), false);
  }
  case N_DOUBLE: {
    (*pos)++;
    if (value)
      break;
    return cup_type_get_double(state);
  }
  case N_STRING:
  case N_MSTRING: {
    (*pos)++;
    if (value)
      break;
    return cup_type_get_array(state, 1,
                              cup_type_get_number(state, sizeof(char), false));
  }
  case N_VARIABLE: {
    n = nodes[(*pos)++];
    n.value = getScopeVar(state, n.value);
    if (n.value == SIZE_MAX)
      break;
    CVariable *var = &(((CVariable *)state->vars.data)[n.value]);
    if (value) {
      if (var->type != value) {
        if (var->type) {
          cup_errorf(state, "defType value VAR %s = %s", CType_name(var->type),
                     CType_name(value));
          exit(1);
        }
        var->type = value;
      }
      return value;
    }
    return var->type;
  }
  case N_UNARY:
  case N_RETURN:
  case N_FUNCTION: {
    (*pos)++;
    if (value)
      goto err;
    return defType(state, nodes, pos, nullptr);
  }
  }

err:
  cup_errorf(state, "defType value " NODEFMT, NODEFMTV(n));
  return nullptr;
}

#define CUPDEFPOWER 2

uint8_t getPower(CTType t) {
  switch (t) {
  case T_OSB:
  default:
    return UINT8_MAX;

  case T_EOF:
    return 0;
  case T_IF:
  case T_RETURN:
  case T_NL:
  case T_OCB:
  case T_CRB:
  case T_CCB:
  case T_CSB:
    return 1;

  case T_COMMA:
    return 2;

  case T_EQ:
  case T_ADDEQ:
  case T_SUBEQ:
  case T_MULEQ:
  case T_DIVEQ:
  case T_MODEQ:
  case T_ANDEQ:
  case T_OREQ:
  case T_XOREQ:
    return 3;

  case T_OR:
  case T_AND:
  case T_XOR:
    return 6;

  case T_EQEQ:
  case T_NOTEQ:
    return 8;

  case T_LESSEQ:
  case T_GREATEQ:
  case T_LESS:
  case T_GREAT:
    return 10;

  case T_ADD:
  case T_SUB:
    return 12;

  case T_MUL:
  case T_DIV:
  case T_MOD:
    return 14;

  case T_DOT:
    return 16;
  case T_ORB:
    return CUPDEFPOWER;
  }
}

static CVector primary(CUPState *state, const CType **return_type,
                       uint8_t mpower) {
  CVector left = {};
  switch (state->type) {
  case T_IMPORT: {
    vector_push_node(&left, &state->node, state->reallocator);
    getToken(state);

    if (state->type == T_STRING) {
      cup_error(state, "Expected Module name after '#'");
      state->reallocator(left.data, 0);
      return vempty;
    }
    vector_push_node(&left, &state->node, state->reallocator);
    vector_push_node(&left, &state->node2, state->reallocator);
    getToken(state);
    cup_error(state, "T_IMPORT not implemented for now");
    // TODO: make T_IMPORT
    return left;
  }
  case T_EXTERNAL: {
    vector_push_node(&left, &state->node, state->reallocator);
    getToken(state);

    if (state->type != T_STRING) {
      cup_error(state, "Expected DLL name after '~'");
      state->reallocator(left.data, 0);
      return vempty;
    }
    vector_push_node(&left, &state->node, state->reallocator);
    vector_push_node(&left, &state->node2, state->reallocator);
    getToken(state);
    cup_error(state, "T_EXTERNAL not implemented for now");
    // TODO: make T_EXTERNAL
    return left;
  }
  case T_AT: { // function
    vector_push_node(&left, &state->node, state->reallocator);
    getToken(state);

    if (state->type == T_AT) {
      getToken(state);
      cup_error(state, "function macro for 'T_AT' not implemented yet");
      state->reallocator(left.data, 0);
      return vempty;
    }
    vector_push_node(&left, &state->node, state->reallocator);
    size_t v = getScopeVar(state, state->value);
    if (v) {
      cup_error(state, "redefined variable function");
      exit(1);
    }
    v = addScopeVar(state, state->value, nullptr, 0);
    {
      Node nt;
      nt.value = v;
      vector_push_node(&left, &nt, state->reallocator);
    }
    expectAndNext(T_IDENTIFIER);
    CVector vars = {state->vars.size, state->vars.size, nullptr};
    vars.data = state->reallocator(nullptr, state->vars.size);
    state->vars.size = 0;
    CVector args_nodes = primary(state, nullptr, CUPDEFPOWER);
    const CType *args_type;
    {
      size_t pos = 0;
      args_type = defType(state, (Node *)args_nodes.data, &pos, nullptr);
    }
    cup_error(state, "TODO: AT");
    exit(1);
    if (state->type == T_NL)
      skipSpaces(state);

    ((CVariable *)vars.data)[v].type =
        cup_type_get_function(state, ABI_DEFAULT, args_type, nullptr);

    fpush_vector(&state->vars, vars, state->reallocator);
    push_vector(&left, args_nodes, state->reallocator);
    { // statement
      const CType **rt = (const CType **)&(
          ((CVariable *)state->vars.data)[v].type->as.func.return_type);
      push_vector(&left, primary(state, rt, CUPDEFPOWER), state->reallocator);
      if (state->node.type == T_NL)
        skipSpaces(state);
    }
    return left;
  }
  case T_BREAK:
  case T_CONTINUE: {
    vector_push_node(&left, &state->node, state->reallocator);
    getToken(state);
    break;
  }
  case T_WHILE: {
    vector_push_node(&left, &state->node, state->reallocator);
    skipSpaces(state);
    push_vector(&left, primary(state, return_type, CUPDEFPOWER),
                state->reallocator);
    if (state->type == T_NL)
      skipSpaces(state);
    { // statement
      push_vector(&left, primary(state, return_type, CUPDEFPOWER),
                  state->reallocator);
      if (state->type == T_NL)
        skipSpaces(state);
      if (state->type == T_EOF)
        cup_error(state, "Expected newline after while statement");
    }
    return left;
  }
  case T_RETURN: {
    vector_push_node(&left, &state->node, state->reallocator);
    skipSpaces(state);
    CVector right = primary(state, nullptr, CUPDEFPOWER);
    if (return_type) {
      size_t pos = 0;
      *return_type = defType(state, (Node *)right.data, &pos, nullptr);
      // cup_error(state, "Not valid return expression");
      // cup_free(left.data);
      // left.data = nullptr;
      // left.capacity = 0;
      // left.size = 0;
      // return {};
    } else {
      cup_error(state, "Return in not returnable expression");
      exit(1);
    }
    push_vector(&left, right, state->reallocator);

    if (return_type) {
      // const CType* type = defType(state, right);
      // if (*return_type && *return_type != type) {
      //   cup_error(state, "Return type mismatch");
      //	exit(1);
      // }
      // else
      //*return_type = type;
    }
    break;
  }
  case T_SUB: {
    Node n = state->node;
    n.type = N_UNARY;
    vector_push_node(&left, &n, state->reallocator);
    getToken(state);
    push_vector(&left, primary(state, return_type, CUPDEFPOWER),
                state->reallocator);
    break;
  }
  case T_NOT: {
    vector_push_node(&left, &state->node, state->reallocator);
    getToken(state);
    push_vector(&left, primary(state, return_type, CUPDEFPOWER),
                state->reallocator);
    break;
  }
  case T_IDENTIFIER: {
    vector_push_node(&left, &state->node, state->reallocator);

    size_t v = getScopeVar(state, state->value);
    if (v != SIZE_MAX) {
      // TODO: check is it conflict name and type
      const char *s = CType_name(((CVariable *)state->vars.data)[v].type);
      cup_errorf(state, "TODO: check is it conflict name and type %s", s);
    } else {
      v = addScopeVar(state, state->value, nullptr, 0);
    }
    state->value = v;
    vector_push_node(&left, &state->node2, state->reallocator);
    getToken(state);
    if (state->type == T_OSB) {
      cup_error(state, "Subscripts not implemented");
      exit(1);
    }
    break;
  }
  case T_STRING: {
    vector_push_node(&left, &state->node, state->reallocator);
    vector_push_node(&left, &state->node2, state->reallocator);
    getToken(state);
    break;
  }
  case T_NUMBER: {
    Node n1 = state->node;
    Node n2 = state->node2;
    getToken(state);
    if (state->type == T_DOT) {
      n1.type = N_DOUBLE;
      n2.dvalue = (double)n2.value;
      getToken(state);
      if (state->type == T_NUMBER) {
        if (state->value != 0)
          n2.dvalue += (state->value / pow(10, floor(log10(state->value) + 1)));
      }
    }
    vector_push_node(&left, &n1, state->reallocator);
    vector_push_node(&left, &n2, state->reallocator);
    break;
  }
  case T_ORB: { // '('
    state->type = N_COMMA;
    vector_push_node(&left, &state->node, state->reallocator);
    vector_push_node(&left, &state->node2, state->reallocator);
    skipSpaces(state);
    if (state->type != T_CRB) {
      push_vector(&left, primary(state, return_type, CUPDEFPOWER),
                  state->reallocator);
      ((Node *)left.data)[1].value = 1;
    }
    if (state->type == T_NL)
      skipSpaces(state);
    expectAndNext(T_CRB);
    break;
  }
  case T_OCB: { // '{'
    state->type = N_BLOCK;
    vector_push_node(&left, &state->node, state->reallocator);
    vector_push_node(&left, &state->node2, state->reallocator);
    skipSpaces(state); // skip T_OCB {
    while (state->type != T_CCB) {
      { // statement
        CVector block = primary(state, return_type, CUPDEFPOWER);
        push_vector(&left, block, state->reallocator);
        if (state->type == T_NL)
          skipSpaces(state);
        if (state->type == T_EOF)
          cup_error(state, "Expected '}' but got EOF");
        if (block.data)
          ((Node *)left.data)[1].value++;
      }
    }
    if (state->node.type == T_NL)
      skipSpaces(state);
    expectAndNext(T_CCB);
    return left;
  }
  case T_OSB: { // '['
    state->node.type = N_ARRAY;
    vector_push_node(&left, &state->node, state->reallocator);
    vector_push_node(&left, &state->node2, state->reallocator);
    skipSpaces(state);
    if (state->type != T_CSB) {
      CVector body = primary(state, return_type, CUPDEFPOWER);
      if (body.data)
        ((Node *)left.data)[1].value++;
      push_vector(&left, body, state->reallocator);
    }
    if (state->node.type == T_NL)
      skipSpaces(state);
    expectAndNext(T_CSB);
    break;
  }
  case T_IF: {
    vector_push_node(&left, &state->node, state->reallocator);
    skipSpaces(state);
    push_vector(&left, primary(state, return_type, CUPDEFPOWER),
                state->reallocator);
    if (state->node.type == T_NL)
      skipSpaces(state);
    { // statement
      push_vector(&left, primary(state, return_type, CUPDEFPOWER),
                  state->reallocator);
      if (state->node.type == T_NL)
        skipSpaces(state);
      if (state->node.type == T_EOF)
        cup_error(state, "Expected newline after while statement");
    }
    if (state->node.type == T_ELSE) {
      vector_fpush_node(&left, &state->node, state->reallocator);
      skipSpaces(state);
      { // statement
        push_vector(&left, primary(state, return_type, CUPDEFPOWER),
                    state->reallocator);
        if (state->node.type == T_NL)
          skipSpaces(state);
        if (state->node.type == T_EOF)
          cup_error(state, "Expected newline after while statement");
      }
    }
    break;
  }
  case T_VARG: {
    vector_push_node(&left, &state->node, state->reallocator);
    getToken(state);
    break;
  }
  default: {
    cup_errorf(state, "primary: Unexpected token " NODEFMT,
               NODEFMTV(state->node));
    exit(1);
    break;
  }
  }

  while (true) {
    Node t = state->node;
    const uint8_t power = getPower(t.type);
    printf("power: %d %s %d\n", mpower, CTType_name(t.type), power);
    if (power == UINT8_MAX) {
      cup_errorf(state, "primary op1: Unexpected token %s",
                 CTType_name(t.type));
      exit(1);
    }
    printf("%d < %d\n", power, mpower);
    if (power < mpower)
      return left;
    skipSpaces(state);
    CVector right = {};
    int norb = t.type != T_ORB;
    if (state->type != T_CRB || norb)
      right = primary(state, return_type, norb ? power : CUPDEFPOWER);
    else {
      Node tempn = t;
      tempn.type = N_COMMA;
      vector_push_node(&right, &tempn, state->reallocator);
      tempn.value = 0;
      vector_push_node(&right, &tempn, state->reallocator);
    }

    switch (t.type) {
    case T_ORB: {
      expectAndNext(T_CRB);
      t.type = N_CALL;
      vector_fpush_node(&left, &t, state->reallocator);
      Node *nptr = (Node *)right.data;
      cup_errorf(state, "CALL: " NODEFMT, NODEFMTV(*nptr));
      if (nptr->type != N_COMMA) {
        t.type = N_COMMA;
        vector_push_node(&left, &t, state->reallocator);
        t.value = 1;
        vector_push_node(&left, &t, state->reallocator);
      }
      break;
    }
    case T_EQ: {
      assert(right.size >= 1 && "Right size is small");
      {
        size_t pos = 0;
        const CType *rv = defType(state, (Node *)right.data, &pos, nullptr);
        pos = 0;
        defType(state, (Node *)left.data, &pos, rv);
      }
      vector_fpush_node(&right, &t, state->reallocator);
      fpush_vector(&left, right, state->reallocator);
      continue;
    }
    case T_COMMA: {
      if (((Node *)left.data)->type == N_COMMA)
        ((Node *)left.data)[1].value++;
      else {
        Node n = {.value = 2};
        vector_fpush_node(&left, &n, state->reallocator);
        vector_fpush_node(&left, &t, state->reallocator);
      }
      break;
    }
    case T_ADD:
    case T_EQEQ:
    case T_MUL:
    case T_LESSEQ:
    case T_SUB: {
      vector_fpush_node(&left, &t, state->reallocator);
      break;
    }
    default:
      cup_errorf(state, "primary op2: Unexpected token " NODEFMT, NODEFMTV(t));
      exit(1);
    }
    push_vector(&left, right, state->reallocator);
  }
}

typedef struct {
  uint8_t call_arg_regs[8];
} ABI;

#if 1

#include "./codegenX64.c"

// Microsoft x64 calling convention
// static X64Reg MicrosoftRegOrder[] = {RCX, RDX, R8, R9};
// Caller-saved registers
// Responsibility: The caller (the function making the call) must save
// them if it needs the values later. They are considered volatile: the
// callee (the function being called) is free to overwrite them without
// restoring. static X64Reg MicrosoftRegCaller[] = {RAX, RCX, RDX, R8, R9,
// R10, R11}; Callee-saved registers Responsibility: The callee (the
// function being called) must preserve them if it wants to use them. On
// function entry, if the callee uses such a register, it must save it
// (usually on the stack) and restore before returning. static X64Reg
// MicrosoftRegCallee[] = {RBX, RBP, RDI, RSI, RSP, R12, R13, R14, R15};

#endif

static Node getNode(Nodes_ *nodes, CUPState *state) {
  if (nodes->pos >= nodes->count) {
    cup_error(state, "Incorrect Nodes");
    exit(1);
  }
  return nodes->nodes[nodes->pos++];
}

static size_t codegen_expr(CUPState *state, Nodes_ *nodes, CBuffer *buf,
                           X64Reg target);

static void codegen_op(CUPState *state, Nodes_ *nodes, CBuffer *buf,
                       X64Reg target, X64Reg temp_reg, const CTType type) {
  size_t i = codegen_expr(state, nodes, buf, target);
  if (i) {
    cup_error(state, "N_OP 1");
    exit(1);
  }

  emit_push_reg(buf, temp_reg);
  emit_push_reg(buf, target);
  i = codegen_expr(state, nodes, buf, temp_reg);
  if (i) {
    cup_error(state, "N_OP 2");
    exit(1);
  }

  emit_pop_reg(buf, target);
  switch (type) {
  case N_LESSEQ:
    emit_cmp_reg_reg(buf, target, temp_reg);
    emit_setle_reg(buf, target);
    break;
  case N_EQEQ:
    emit_cmp_reg_reg(buf, target, temp_reg);
    emit_sete_reg(buf, target);
    break;
  case N_MUL:
    emit_imul_reg_reg(buf, target, temp_reg);
    break;
  case N_ADD:
    emit_add_reg_reg(buf, target, temp_reg);
    break;
  case N_SUB:
    emit_sub_reg_reg(buf, target, temp_reg);
    break;
  }
  emit_pop_reg(buf, temp_reg);
}

static const X64Reg sysv_arg_regs[] = {RDI, RSI, RDX, RCX, R8, R9};

static size_t codegen_expr(CUPState *state, Nodes_ *nodes, CBuffer *buf,
                           X64Reg target) {
  // Nodes nodes, CBuffer *buf, size_t *pos,
  //                            X64Reg target_reg, int left) {

  Node node = getNode(nodes, state);

  switch (node.type) {
  case N_BLOCK:
  case N_COMMA: {
    return getNode(nodes, state).value;
  }
  case N_NUMBER: {
    emit_mov_reg_imm64(buf, target, getNode(nodes, state).value);
    break;
  }
  case N_VARIABLE: {
    state->value = getNode(nodes, state).value;
    state->variable = &((CVariable *)state->vars.data)[state->value];
    emit_mov_reg_imm64(buf, target, (size_t)&(state->variable->value));
    emit_mov_reg_deref(buf, target, target);
    break;
  }
  case N_CALL: {
    // System V AMD64 ABI calling convention
    // Arguments in order: RDI, RSI, RDX, RCX, R8, R9, then stack
    // RAX contains return value
    // Caller-saved: RAX, RCX, RDX, RSI, RDI, R8, R9, R10, R11

    size_t save = nodes->pos;
    const CType *type = defType(state, nodes->nodes, &save, nullptr);
    size_t i = codegen_expr(state, nodes, buf, RAX);
    if (i) {
      cup_error(state, "N_CALL 1");
      exit(1);
    }
    // Check if there's a N_COMMA node with arguments
    uint8_t count = type->as.func.args->as.complex.count;
    printf("call = %d, %i\n", count, type->type);
    if (count > 6) {
      cup_error(state, "More than 6 arguments not yet supported in JIT");
      count = 6;
    }

    emit_push_reg(buf, RAX);
    i = codegen_expr(state, nodes, buf, RAX);
    if (i != count) {
      cup_errorf(state, "N_CALL ARGS %ld %ld", i, count);
      exit(1);
    }
    printf("i = %ld\n", i);
    for (i = 0; i < count; i++) {
      if (codegen_expr(state, nodes, buf, sysv_arg_regs[i])) {
        cup_error(state, "N_CALL 1 (REAL)");
        exit(1);
      }
    }
    emit_pop_reg(buf, RAX);

    emit_call_reg(buf, RAX);
    if (target != RAX) {
      emit_mov_reg_reg(buf, target, RAX);
    }
    break;
  }

  case N_ADD:
  case N_SUB:
  case N_MUL:
  case N_EQEQ:
  case N_LESSEQ: {
    codegen_op(state, nodes, buf, target, R10, node.type);
    break;
  }
  default:
    cup_errorf(state, "codegen UNKNOWN node " NODEFMT "\n", NODEFMTV(node));
    exit(1);
  }
  return 0;
}

static size_t codegen_left(Nodes_ *nodes, CBuffer *buf, X64Reg target);

static void codegen_func(CBuffer *buf, Nodes_ *nodes, size_t *value, size_t i) {
  CBuffer bnew = {};
  bnew.state = buf->state;
  printf("---------\n");
  // size_t i = codegen_expr(buf->state, nodes, nullptr, RAX);
  // CVariable *var = buf->state->variable;
  emit_push_reg(&bnew, RBP);
  emit_mov_reg_reg(&bnew, RBP, RSP);
  emit_xor_reg_reg(&bnew, RAX, RAX);
  if (i)
    i = codegen_expr(buf->state, nodes, nullptr, RAX);
  printf("args count = %ld\n", i);
  for (size_t j = 0; j < i; j++)
    codegen_left(nodes, &bnew, sysv_arg_regs[j]);
  i = codegen_left(nodes, &bnew, RAX);
  for (size_t j = 0; j < i; j++)
    codegen_left(nodes, &bnew, RAX);
  emit_mov_reg_reg(&bnew, RSP, RBP);
  emit_pop_reg(&bnew, RBP);
  emit_ret(&bnew);
  printf("---------\n");

  {
    emit_error(&bnew, buf->size);
    memcpy((uint8_t *)bnew.data + bnew.size, buf->data, buf->size);
    buf->state->reallocator(buf->data, 0);
    for (CRealloc *ptr = (CRealloc *)buf->reallocs.data;
         (size_t)ptr < buf->reallocs.size; ptr++)
      ptr[i].ptr += bnew.size;
    CRealloc r = {value, 0, 0};
    vector_push(&buf->reallocs, &r, sizeof(r), buf->state->reallocator);
    bnew.size += buf->size;
    push_vector(&bnew.reallocs, buf->reallocs, buf->state->reallocator);
    memcpy(buf, &bnew, sizeof(bnew));
  }
}

static size_t codegen_left(Nodes_ *nodes, CBuffer *buf, X64Reg target) {
  CUPState *state = buf->state;
  Node node = getNode(nodes, state);
  switch (node.type) {
  default:
    cup_errorf(state, "codegen_left: UNKNOWN node (" NODEFMT ")",
               NODEFMTV(node));
    break;
  case N_BLOCK:
  case N_COMMA: {
    return getNode(nodes, state).value;
  }
  case N_NUMBER: {
    emit_mov_reg_imm64(buf, target, getNode(nodes, state).value);
    break;
  }
  case N_VARIABLE: {
    state->value = getNode(nodes, state).value;
    state->variable = &((CVariable *)state->vars.data)[state->value];
    emit_mov_reg_imm64(buf, target, (size_t)&(state->variable->value));
    break;
  }
  case N_RETURN: {
    size_t i = codegen_expr(state, nodes, buf, RAX);
    if (i) {
      cup_error(state, "N_RETURN 1");
      exit(i);
    }

    emit_mov_reg_reg(buf, RSP, RBP);
    emit_pop_reg(buf, RBP);
    emit_ret(buf);
    break;
  }
  case N_ASSIGN: {
    size_t i = codegen_left(nodes, buf, RAX);
    if (i) {
      cup_error(state, "N_ASSIGN 1");
      exit(1);
    }
    if (target == RAX) {
      emit_push_reg(buf, RBX);
      emit_push_reg(buf, target);
    }
    i = codegen_left(nodes, buf, target);
    if (i) {
      cup_error(state, "N_ASSIGN 1");
      exit(1);
    }
    if (target == RAX) {
      emit_pop_reg(buf, RBX);
      emit_mov_deref_reg(buf, RBX, target);
      emit_pop_reg(buf, RBX);
    } else {
      emit_mov_deref_reg(buf, RAX, target);
    }
    break;
  }
  case N_IF: {
    // Evaluate condition into target_reg
    size_t i = codegen_expr(state, nodes, buf, target);
    if (i) {
      cup_error(state, "N_IF 1");
      exit(1);
    }

    // TEST target_reg, target_reg (check if zero)
    emit_test_reg_reg(buf, target, target);

    // Reserve space for JE (jump if equal/zero) - will patch later
    emit8(buf, 0xFF);
    emit8(buf, 0x25);
    size_t je_offset = buf->size;
    emit32(buf, 0);
    // Generate 'then' body
    codegen_left(nodes, buf, RAX);
    // Patch JE to here
    *(int32_t *)(buf->data + je_offset) =
        buf->size - (je_offset + sizeof(int32_t));
    break;
  }
  case N_FUNCTION: {
    size_t i = codegen_expr(state, nodes, nullptr, RAX);
    if (i) {
      cup_error(state, "N_FUNCTION 1");
      exit(i);
    }
    CVariable *var = state->variable;
    codegen_func(buf, nodes, &var->value, SIZE_MAX);
    break;
  }
  case N_CALL: {
    // System V AMD64 ABI calling convention
    // Arguments in order: RDI, RSI, RDX, RCX, R8, R9, then stack
    // RAX contains return value
    // Caller-saved: RAX, RCX, RDX, RSI, RDI, R8, R9, R10, R11

    size_t save = nodes->pos;
    const CType *type = defType(state, nodes->nodes, &save, nullptr);
    size_t i = codegen_expr(state, nodes, buf, RAX);
    if (i) {
      cup_error(state, "N_CALL 1");
      exit(1);
    }
    // Check if there's a N_COMMA node with arguments
    uint8_t count = type->as.func.args->as.complex.count;
    printf("call = %d, %i\n ", count, type->type);
    if (count > 6) {
      cup_error(state, "More than 6 arguments not yet supported in JIT");
      count = 6;
    }

    emit_push_reg(buf, RAX);
    i = codegen_expr(state, nodes, nullptr, RAX);
    if (i != count) {
      cup_errorf(state, "N_CALL ARGS %ld %ld", i, count);
      exit(1);
    }
    printf("i = %ld\n", i);
    for (i = 0; i < count; i++) {
      if (codegen_expr(state, nodes, buf, sysv_arg_regs[i])) {
        cup_error(state, "N_CALL 1 (REAL)");
        exit(1);
      }
    }
    emit_pop_reg(buf, RAX);

    emit_call_reg(buf, RAX);
    if (target != RAX) {
      emit_mov_reg_reg(buf, target, RAX);
    }
    break;
  }
  }
  return 0;
}

static void printNodes(CUPState *state, Node *nodes, size_t count) {
  for (size_t i = 0; i < count; i++) {
    Node n = nodes[i];
    printf(NODEFMT "\n", NODEFMTV(n));
    if (n.type == N_VARIABLE) {
      CVariable v = ((CVariable *)state->vars.data)[nodes[++i].value];
      printf("%s\n", getString(state, v.name));
    } else if (n.type == N_NUMBER || n.type == N_COMMA || n.type == N_BLOCK) {
      printf("%ld\n", nodes[++i].value);
    }
  }
}

static void parse(CUPState *state) {
  CVector nodes = {};

  state->type = N_BLOCK;
  vector_push_node(&nodes, &state->node, state->reallocator);
  state->value = 0;
  vector_push_node(&nodes, &state->node2, state->reallocator);

  const CType *type = nullptr;
  skipSpaces(state);
  while (state->type != T_EOF) {
    CUPState save = *state;
    CVector n = primary(state, &type, CUPDEFPOWER);
    printf("parse: %s %.*s\nend\n", CTType_name(state->type),
           (int)(state->input_stream - save.input_stream), save.input_stream);
    size_t pos = 0;
    defType(state, (Node *)n.data, &pos, nullptr);
    push_vector(&nodes, n, state->reallocator);

    if (state->type == T_NL)
      skipSpaces(state);
    if (n.data)
      ((Node *)nodes.data)[1].value++;
  }
  if (type) {
    printf("type = %s\n", CType_name(type));
  }

  if (1) {
    Node *ptr = (Node *)nodes.data;
    size_t count = nodes.size / sizeof(Node);
    printNodes(state, ptr, count);
    count = state->vars.size / sizeof(CVariable);
    printf("\nCVariable size = %ld\n", count);
    while (count--) {
      CVariable v = ((CVariable *)state->vars.data)[count];
      size_t s = cup_type_size(v.type);
      printf("Var: [%ld]%s type= %s, ptr= %ld, size = %ld\n", v.name,
             getString(state, v.name), CType_name(v.type), (size_t)v.value, s);
    }
    printf("--\n");
    // codegen
    size_t init = 0;
    CBuffer buf = {};
    buf.state = state;
    Nodes_ nnodes = {(Node *)nodes.data, nodes.size / sizeof(Node), 0};
    codegen_func(&buf, &nnodes, &init, 0);
    void *page = allocMemory(buf.size);
    memcpy(page, buf.data, buf.size);
    state->reallocator(buf.data, 0);
    uint8_t *prr = (uint8_t *)page;
    printf("prr = %p\n", prr);
    CRealloc *rptr = (CRealloc *)buf.reallocs.data;
    for (size_t i = 0; i < (buf.reallocs.size / sizeof(CRealloc)); i++) {
      CRealloc rr = rptr[i];
      if (prr[rr.ptr] == 0x55) {
        printf("BR: %ld\n", *rr.var);
        *rr.var = (size_t)(prr + rr.ptr);
        printf("AR: %ld\n", *rr.var);
      }
      printf("[%ld] %ld %p (%02X) value = %ld\n", i, rr.ptr, rr.var,
             prr[rr.ptr], *rr.var);
    }
    for (size_t i = 0; i < (buf.reallocs.size / sizeof(CRealloc)); i++) {
      CRealloc rr = rptr[i];
      if (prr[rr.ptr] == 0) {
        memcpy(prr + rr.ptr, rr.var, sizeof(size_t));
      }
    }
    for (size_t i = 0; i < buf.size; i++) {
      fprintf(stdout, "%02X", ((uint8_t *)page)[i]);
    }

    printf("\n------\n");
    typedef size_t (*JitFunc)();
    JitFunc f = (JitFunc)(init);
    f();

    //   cup_error(state, "Failed to codegen");
    // }
  }
}

#if 1

LIBCUPAPI CUPState *cup_new(CUPReallocFunc *reallocator,
                            CUPErrorFunc *error_func) {
  if (reallocator == nullptr)
    reallocator = default_reallocator;
  CUPState *state = callocT(CUPState, reallocator);
  state->reallocator = reallocator;
  state->error_func = error_func ? error_func : default_error_func;
  state->names = (uint8_t *)callocT(uint16_t, reallocator);
  state->names += 2;
  return state;
}
LIBCUPAPI void cup_delete(CUPState *s) {
  if (!s)
    return;
  CUPReallocFunc *reallocator = s->reallocator;
  for (size_t count = s->pages.size / sizeof(Page); count--;) {
    Page p = ((Page *)s->pages.data)[count];
    freeMemory(p.data, p.size);
  }
  reallocator(s->vars.data, 0);
  reallocator(s->data, 0);
  reallocator(s, 0);
}

LIBCUPAPI void set_machine_type(CUPState *s, CUPMachineType type) {
  (void)type;
  if (s == nullptr)
    return;
  cup_error(s, "Not implemented yet");
}

LIBCUPAPI void cup_set_error_func(CUPState *s, CUPErrorFunc *error_func) {
  s->error_func = error_func ? error_func : default_error_func;
}
LIBCUPAPI void cup_set_realloc(CUPState *s, CUPReallocFunc *my_realloc) {
  s->reallocator = my_realloc ? my_realloc : default_reallocator;
}

LIBCUPAPI void cup_set_lib_path(CUPState *s, const char *path) {
  (void)path;
  if (s == nullptr)
    return;
  cup_error(s, "Not implemented yet");
}

LIBCUPAPI int cup_compile_string(CUPState *s, const char *buf) {
  if (s == nullptr)
    return -1;
  if (buf == nullptr || *buf == '\0') {
    cup_error(s, "Incorrect input stream");
    return -1;
  }
  s->input_stream = buf;
  // s->input_path = "-";
  s->node.loc.col = 1;
  s->node.loc.line = 1;
  parse(s);
  return 0;
}
LIBCUPAPI int cup_compile_file(CUPState *s, const char *filename) {
  if (filename == nullptr || *filename == '\0') {
    cup_error(s, "Incorrect input file path");
    return -1;
  }
  FILE *f = fopen(filename, "rb");

  if (f == nullptr) {
    cup_errorf(s, "File '%s' not found", filename);
    return -1;
  }

  fseek(f, 0, SEEK_END);
  long size = ftell(f);
  fseek(f, 0, SEEK_SET);
  void *buf = s->reallocator(nullptr, size + 1);
  ((char *)buf)[size] = '\0';
  fread(buf, size, 1, f);
  fclose(f);

  s->input_stream = (char *)buf;
  printf("\n%s\n", s->input_stream);
  // s->input_path = filename;
  s->node.loc.col = 1;
  s->node.loc.line = 1;
  parse(s);
  s->reallocator(buf, 0);
  return 0;
}
LIBCUPAPI int cup_add_library(CUPState *s, const char *pathname) {
  (void)pathname;
  if (s == nullptr)
    return -1;
  cup_error(s, "Not implemented yet");
  return -1;
}
LIBCUPAPI int cup_add_symbol(CUPState *state, const char *name, void *val,
                             const CType *type) {
  // TODO: add check safe type
  if (state == nullptr)
    return -1;
  if (name == nullptr || *name == '\0')
    return -1;
  size_t len = strlen(name);
  idToken(state, name, len);

  size_t v = getScopeVar(state, state->value);
  if (v != SIZE_MAX) {
    cup_errorf(state, "Cannot add symbol(%s) because it already exist", name);
    return -1;
  }
  v = addScopeVar(state, state->value, type, (size_t)val);
  return 0;
}
LIBCUPAPI void *cup_get_symbol(CUPState *s, const char *name) {
  if (s == nullptr)
    return nullptr;
  if (name == nullptr || *name == '\0')
    return nullptr;
  size_t len = strlen(name);
  idToken(s, name, len);

  size_t v = getScopeVar(s, s->value);
  if (v == SIZE_MAX)
    return nullptr;
  return &(((CVariable *)s->vars.data)[v].value);
}

LIBCUPAPI const CType *cup_type_get_void(CUPState *state) {
  CType t = {R_VOID, {}};
  return cup_type_get(state, t);
}
LIBCUPAPI const CType *cup_type_get_number(CUPState *state, uint8_t size,
                                           int issigned) {
  CType t = {R_NUMBER, {}};
  if (size == 1)
    t.as.num.flags = (issigned ? CTypeN_INT8 : CTypeN_UINT8);
  else if (size == sizeof(size_t))
    t.as.num.flags = CTypeN_SIZE;
  return cup_type_get(state, t);
}
LIBCUPAPI const CType *cup_type_get_double(CUPState *state) {
  CType t = {R_DOUBLE, {}};
  return cup_type_get(state, t);
}
LIBCUPAPI const CType *cup_type_get_pointer(CUPState *state,
                                            const CType *ptrtype) {
  CType t = {R_POINTER, {}};
  t.as.ptr.ptrtype = ptrtype;
  return cup_type_get(state, t);
}
LIBCUPAPI const CType *cup_type_get_array(CUPState *state, size_t count,
                                          const CType *ptrtype) {
  CType t = {R_ARRAY, {}};
  t.as.array.count = count;
  t.as.array.ptrtype = ptrtype;
  return cup_type_get(state, t);
}
LIBCUPAPI const CType *cup_type_get_complex(CUPState *state, uint8_t count,
                                            const CType **types) {
  CType t = {R_COMPLEX, {}};
  t.as.complex.count = count;
  if (types)
    memcpy(t.as.complex.types, types, count * sizeof(*types));
  return cup_type_get(state, t);
}
LIBCUPAPI const CType *cup_type_get_function(CUPState *state, uint8_t abi,
                                             const CType *args,
                                             const CType *return_type) {
  CType t = {R_FUNCTION, {}};
  t.as.func.abi = abi;
  t.as.func.args = args;
  t.as.func.return_type = return_type;
  return cup_type_get(state, t);
}

#endif
