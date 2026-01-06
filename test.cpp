#include "./libcup.h"
#include <stdio.h>
#include <stdlib.h>

void myprint(size_t v) { printf("%ld\n", v); }

int main(int argc, char **argv) {
  CUPState *state = cup_new();
  if (!state) {
    fprintf(stderr, "cup_new failed\n");
    return 1;
  }

  const CType *args[] = {cup_type_get_number(state, sizeof(size_t), false)};
  cup_add_symbol(state, "print", (void *)myprint,
                 cup_type_get_function(state, ABI_DEFAULT, 1, args,
                                       cup_type_get_void(state)));

  const char *test = "a = 0\n"
                     "b = 1\n"
                     "c = 1\n"
                     "a = b + c\n"
                     "c = b\n"
                     "b = a\n"
                     "a = b + c\n"
                     "c = b\n"
                     "b = a\n"
                     "a = b + c\n"
                     "c = b\n"
                     "b = a\n"
                     "print(a)";
  if (cup_compile_string(state, test)) {
    cup_delete(state);
    return 1;
  }
  printf("OK\n");
  return 0;
}
