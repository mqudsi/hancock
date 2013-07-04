#include "sfio.h"

typedef struct{
  int i;
  unsigned int j;
} intPair;

typedef struct{
  intPair one;
  char c;
  intPair two;
  float f;
  double d;
  long l;
} grabBag_t;

void writeGrabBag(int oi, unsigned int oj,
                  char c,
                  int ti, unsigned int tj,
                  float f, double d, long l){
  grabBag_t p;
  p.one.i = oi;
  p.one.j = oj;
  p.c = c;
  p.two.i = ti;
  p.two.j = tj;
  p.f = f;
  p.d = d;
  p.l = l;
  sfwrite(sfstdout, (void const *)&p, sizeof(p));
}

int main(){
  writeGrabBag(-1, 1, 'a', 10, 12, 120.3, 12.4, 100);
  writeGrabBag(-1, 2, 'b', 11, 13, 130.3, 15.4, 101);
  writeGrabBag(-1, 2, 'b', 11, 13, 130.3, -5.4, 101);
  writeGrabBag(-1, 2, 'b', 11, 13, 130.3, 14.4, 101);
  writeGrabBag(-1, 2, 'b', 11, 17, 130.3, -4.4, 101);
  writeGrabBag(-1, 2, 'b', 11, 19, 130.3, 19.4, 101);
  writeGrabBag(-1, 7, 'b', 10, 15, -13.3, 14.4, -11);
  writeGrabBag(10, 2, 'd', 18, 15, -17.3, 10.4, -10);
  writeGrabBag(14, 8, 'a', -9, 99, 107.3, -0.4, 356);
  writeGrabBag(14, 3, 'a', -9, 99, 107.3, -0.4, 356);
  writeGrabBag(12, 9, 'a', -9, 99, 107.3, 10.4, 356);
  writeGrabBag(12, 9, 'a', -9, 99, 107.3, -0.4, 356);
  writeGrabBag(14, 9, -12, -9, 97, 107.3, -0.4, 356);

  return 0;
}
