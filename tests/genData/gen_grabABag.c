#include "sfio.h"

typedef struct{
  int i;
  unsigned int j;
} intPair;

enum color {red, blue, green};

#define WORD_SIZE 4

typedef struct{
  char word[WORD_SIZE];
  enum color e;
  intPair one;
  char c;
  intPair two;
  float f;
  double d;
  long l;
} grabBag_t;

void writeGrabBag(char *word, 
		  enum color e,
                  int oi, unsigned int oj,
                  char c,
                  int ti, unsigned int tj,
                  float f, double d, long l){
  grabBag_t p;
  int i;
  for(i=0; i<WORD_SIZE; i++)
    p.word[i] = word[i];
  p.e = e;
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
  writeGrabBag("cat", red,  -1, 1, 'a', 10, 12, 120.3, 12.4, 100);
  writeGrabBag("cat", blue, -1, 2, 'b', 11, 13, 130.3, 15.4, 101);
  writeGrabBag("dog", blue, -1, 2, 'b', 11, 13, 130.3, -5.4, 101);
  writeGrabBag("cat", green,-1, 2, 'b', 11, 13, 130.3, 14.4, 101);
  writeGrabBag("bat", blue, -1, 2, 'b', 11, 17, 130.3, -4.4, 101);
  writeGrabBag("cat", red,  -1, 2, 'b', 11, 19, 130.3, 19.4, 101);
  writeGrabBag("cat", green,-1, 7, 'b', 10, 15, -13.3, 14.4, -11);
  writeGrabBag("red", blue, 10, 2, 'd', 18, 15, -17.3, 10.4, -10);
  writeGrabBag("cat", blue, 14, 8, 'a', -9, 99, 107.3, -0.4, 356);
  writeGrabBag("pop", red,  14, 3, 'a', -9, 99, 107.3, -0.4, 356);
  writeGrabBag("cat", blue, 12, 9, 'a', -9, 99, 107.3, 10.4, 356);
  writeGrabBag("top", blue, 2, 9, 'a', -9, 99, 107.3, -0.4, 356);
  writeGrabBag("cat", blue, 14, 9, -12, -9, 97, 107.3, -0.4, 356);

  return 0;
}
