#include "./libcup.h"
#include <stdio.h>
#include <stdlib.h>

static void printUsage() {
  puts("Usage:\tcup [options] <file>\nOptions:\n"
       "-? / -h\t\tShow this help\n"
       "-v\t\tShow the version\n"
       "-n\t\tNo warnings\n"
       "-x\t\tCompile x32\n"
       "-d\t\tAdd DebugInfo\n");
}

void myprint(size_t v) { printf("%ld\n", v); }

int main(int argc, char **argv) {
  if (argc < 2) {
    printUsage();
    return 0;
  }

  CUPState *state = cup_new(nullptr, nullptr);
  if (!state) {
    fprintf(stderr, "cup_new failed\n");
    return 1;
  }

  const CType *args[] = {
      cup_type_get_number(state, sizeof(size_t), false),
  };
  const CType *args_type = cup_type_get_complex(state, 1, args);
  const CType *vt = cup_type_get_void(state);
  const CType *ft = cup_type_get_function(state, ABI_DEFAULT, args_type, vt);
  cup_add_symbol(state, "print", (void *)myprint, ft);

  for (int i = 1; i < argc; i++) {
    if (cup_compile_file(state, argv[i])) {
      cup_delete(state);
      return 1;
    }
  }
  printf("OK\n");
  return 0;
}
