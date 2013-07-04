#include "../scampRec.hh"

int indent = 0;
int indent_space = 4;
#define PR(A) (sfprintf(sfstdout,"%*s",indent,""), (A))

map map_t{
  key ppn_t;
  value int;
  default 2;
};

void set_pn(line_t *to, int npa, int nxx, int line){
   to->npa = npa;
   to->nxx = nxx;
   to->line = line;
}

void do_exchange(map_t myMap, lpn_t start, lpn_t stop){
  iterate(over myMap [start .. stop]
	  withevents mapDetect) {
    
    event line_begin(lpn_t m){
      PR(sfprintf(sfstdout,"%04d: ",m.line ));
      sfprintf(sfstdout,"%*s",indent,"");
      sfprintf(sfstdout,"%d\n", myMap<:m:>);
    }
  } 
}


void test(map_t myMap, lpn_t start, lpn_t stop){
  iterate(over myMap [start .. stop]
	  withevents lineDetect) {

    event nxx_begin(exchange_t exn){
      PR(sfprintf(sfstdout,"(%03d)%03d-%04d:\n",exn.npa,exn.nxx,0));
      set_pn(&start, exn.npa, exn.nxx, 0000);
      set_pn(&stop,  exn.npa, exn.nxx, 9999);
      do_exchange(myMap, start, stop);
    }
  }
}

void sig_main(exists map_t myMap <M:>){
  lpn_t firstB = {200, 305, 0000};

  lpn_t firstE = {200, 545, 9999};
  lpn_t secondE = {201, 302, 9999};
  
  sfprintf(sfstdout, "First test.\n");
  test(myMap, firstB, firstE);
  sfprintf(sfstdout, "Second test.\n");
  test(myMap, firstB, secondE);

}
