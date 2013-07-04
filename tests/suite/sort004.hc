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
   sfprintf(sfstdout, "i=%d, j=%u   ", p.i, p.j);
}

void sig_main(grabBag_s data <c:, "Binary grab bag stream">){
  iterate 
    (over data
     sortedby one, two, d
     withevents getGrabBag){
   
    event theBag(grabBag_t p){
      sfprintf(sfstdout, "First: ");
      printPair(p.one);
      sfprintf(sfstdout, "c=%c ", p.c);
      sfprintf(sfstdout, "Second: ");
      printPair(p.two);
      sfprintf(sfstdout, "f=%f ", p.f);
      sfprintf(sfstdout, "d=%f ", p.d);
      sfprintf(sfstdout, "l=%d ", p.l);
      sfprintf(sfstdout, "\n");
    }
  }

}
