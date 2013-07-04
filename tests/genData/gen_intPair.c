#include "sfio.h"

typedef struct{
  int i;
  int j;
} intPair;

void writePair(int i, int j){
  intPair p;
  p.i = i;
  p.j = j;
  sfwrite(sfstdout, (void const *) &p, sizeof(p));
}

int main(){
  writePair(-1,10);
  writePair(1,10);
  writePair(-1,-10);
  writePair(-5,-10);
  writePair(-5,10);
  writePair(-5,15);

  return 0;
}
