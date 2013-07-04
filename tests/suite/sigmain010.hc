#include "../scampRec.hh"

typedef struct {
  int in;
  int out;
  int tollFree;
} usage_t;

map usage_m{
  key ppn_t;
  value usage_t;
  default {0,0,0};
};

typedef cdStream callDetail_s;

int sig_main(            callDetail_s calls <c:>,
              exists const usage_m      oldUsage <u:, "yesterday's usage map"> default "data/sgMap",
              new          usage_m      newUsage <U:, "today's usage map"> default oldUsage)
{ 
   sfprintf(sfstdout, "Hello.\n");
}
