
typedef struct {
  char c;
} test;

int f(Sfio_t *file, test *h){
  return (sfscanf(file,"%c", &(h->c)) == 1);
}


stream foo_t {
  f: Sfio_t  => test;
};



void goo(foo_t myStream){
  
  iterate(over myStream
          sorted) {

    event (test *c){
      sfprintf(sfstdout,"%c",c->c);
    }
  }

  sfprintf(sfstdout, "\n");
}

void sig_main(foo_t myStream <s:>){
  goo(myStream);
}




