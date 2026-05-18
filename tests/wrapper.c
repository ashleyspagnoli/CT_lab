#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
    
extern int64_t func(int64_t);
    
int main(int argc, char *argv[]) {
  int64_t inp = atoll(argv[1]);
  int64_t out = func(inp);
  printf("%ld\n", out);
  return 0;
}