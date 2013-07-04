typedef struct{
  int i;
  unsigned int j;
} intPair;

typedef double myDouble;

enum color {red, blue, green};

#define WORD_SIZE 4

typedef struct{
  char word[WORD_SIZE];
  enum color e;
  intPair one;
  char c;
  intPair two;
  float f;
  myDouble d;
  long l;
} grabBag_t;

int getValidGrabBag(grabBag_t *p, grabBag_t* l){
    *l = *p;
    return 1;
}

stream grabBag_s {
  getValidGrabBag: grabBag_t => grabBag_t;
}

munion grabBag_e {: grabBag_t theBag :};

grabBag_e getGrabBag(grabBag_t *w[1:0]){
  return {: theBag = *w[0] :};
}

void printPair(intPair p){
   sfprintf(sfstdout, "i=%2d, j=%u   ", p.i, p.j);
}

void printEnum(enum color e){
  switch (e){
  case red:    sfprintf(sfstdout, "red   "); break;
  case blue:    sfprintf(sfstdout, "blue  "); break;
  case green:    sfprintf(sfstdout, "green "); break;
  }
}

void sig_main(grabBag_s data <c:, "Binary A grab bag stream">){
  iterate 
    (over data
     sortedby d,e
     withevents getGrabBag){
   
    event theBag(grabBag_t p){
      sfprintf(sfstdout, "%s ", p.word);
      printEnum(p.e);
      sfprintf(sfstdout, "First: ");
      printPair(p.one);
      sfprintf(sfstdout, "c=%c ", p.c);
      sfprintf(sfstdout, "Second: ");
      printPair(p.two);
      sfprintf(sfstdout, "f=%3.2f ", p.f);
      sfprintf(sfstdout, "d=%3.2f ", p.d);
      sfprintf(sfstdout, "l=%d ", p.l);
      sfprintf(sfstdout, "\n");
    }
  }

}
