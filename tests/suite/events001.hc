
#include <stdio.h>

struct foo { int npa; };

munion line_e {: int npa, char nxx, long line :}
munion cd_e {: int duration, line_e o, line_e d, line_e t, int connect :};

struct bar { int npa; };

int test() {


  struct foo x = {0};
  struct bar y = {0};

  line_e z= {: npa = 5 :};

  if (z.npa!=0) {
    printf("Hello");
  }
  return 1;
}







