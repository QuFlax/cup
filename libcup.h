#ifndef LIBCUP_H
#define LIBCUP_H

#ifndef LIBCUPAPI
#define LIBCUPAPI
#endif

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

enum { CEL_NONE, CEL_INFO, CEL_WARN, CEL_ERRO };
typedef uint8_t CUPErrorLevel;

typedef void *CUPReallocFunc(void *ptr, size_t size);
typedef void CUPErrorFunc(CUPErrorLevel level, const char *msg);

typedef struct CUPState CUPState;
LIBCUPAPI CUPState *cup_new(CUPReallocFunc *, CUPErrorFunc *error_func);
LIBCUPAPI void cup_delete(CUPState *);

enum {
  CIRM_UNKNOWN,
  CIRM_X64,
};
typedef uint8_t CUPMachineType;

LIBCUPAPI void set_machine_type(CUPState *, CUPMachineType);
LIBCUPAPI void cup_set_error_func(CUPState *s, CUPErrorFunc *error_func);
LIBCUPAPI void cup_set_realloc(CUPState *s, CUPReallocFunc *my_realloc);

LIBCUPAPI int cup_compile_string(CUPState *s, const char *buf);
LIBCUPAPI int cup_compile_file(CUPState *s, const char *filename);

#define CUP_ERROR_CTYPE nullptr
typedef struct CType CType;

enum {
  CTypeN_SIZE,
  CTypeN_UINT8,
  CTypeN_INT8,
};
typedef uint8_t CTypeNType;

typedef struct {
  CTypeNType flags;
} CTypeNumber;

typedef struct {
  uint8_t abi;
  const CType *return_type;
  const CType *args;
} CTypeFunc;

typedef struct {
  const CType *ptrtype;
} CTypePointer;

typedef struct {
  size_t count;
  const CType *ptrtype;
} CTypeArray;

typedef struct {
  uint8_t count;
  const CType *types[UINT8_MAX];
} CTypeComplex;

struct CType {
  uint8_t type;
  union As {
    CTypeNumber num;
    CTypeFunc func;
    CTypePointer ptr;
    CTypeArray array;
    CTypeComplex complex;
  } as;
};

#define ABI_SYSV 1
#define ABI_MX64 2
#ifndef _WIN32
#define ABI_DEFAULT ABI_SYSV
#else
#define AABI_DEFAULT ABI_MX64
#endif // DEBUG

LIBCUPAPI const CType *cup_type_get_void(CUPState *state);
LIBCUPAPI const CType *cup_type_get_number(CUPState *state, uint8_t size,
                                           int issigned);
LIBCUPAPI const CType *cup_type_get_double(CUPState *state);
LIBCUPAPI const CType *cup_type_get_pointer(CUPState *state,
                                            const CType *ptrtype);
LIBCUPAPI const CType *cup_type_get_array(CUPState *state, size_t count,
                                          const CType *ptrtype);
LIBCUPAPI const CType *cup_type_get_complex(CUPState *state, uint8_t count,
                                            const CType *types[UINT8_MAX]);
LIBCUPAPI const CType *cup_type_get_function(CUPState *state, uint8_t abi,
                                             const CType *args,
                                             const CType *return_type);
// LIBCUPAPI const CType *cup_type_get_complex(struct CComplexExtra *extra,
// size_t size);

LIBCUPAPI int cup_add_symbol(CUPState *s, const char *name, void *val,
                             const CType *type);
LIBCUPAPI void *cup_get_symbol(CUPState *s, const char *name);

#ifdef __cplusplus
}
#endif

#endif
