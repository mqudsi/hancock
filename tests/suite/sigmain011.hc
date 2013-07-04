#include <float.h>

void sig_main(float f <f:>,
              float g default 2.4 <g:>){

  sfprintf(sfstdout, "The value of f is: %3.2f.\n", f);
  sfprintf(sfstdout, "The value of g is: %3.2f.\n", g);

}
