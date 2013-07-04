#include "../scampRec.hh"

typedef struct {
  char c;
} test;

int f(Sfio_t *file, test *h){
  return (sfscanf(file,"%c", &(h->c)) == 1);
}


stream foo_t {
  f: Sfio_t  => test;
};

munion always_e {: char click :};

always_e do_click(test *r[1:0]) {
  return {: click=r[0]->c :};
}

void goo(foo_t myStream){
  
  iterate(over myStream
          sorted
	  withevents do_click) {

    event click(char c){
      sfprintf(sfstdout,"%c",c);
    }
  }

  sfprintf(sfstdout, "\n");
}

void sig_main(foo_t myStream <s:>){
  goo(myStream);
}




