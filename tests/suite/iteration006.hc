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

int noEven(lpn_t * pn){
   return pn->line % 2;
}

void test(map_t myMap, lpn_t start, lpn_t stop){
  iterate(over myMap [start .. stop]
          filteredby noEven
          sortedby nxx
	  withevents mapDetect) {
    
    event line_begin(lpn_t m){
      sfprintf(sfstdout,"(%03d)%03d-%04d: ",m.npa,m.nxx,m.line);
      sfprintf(sfstdout,"%*s",indent,"");
      sfprintf(sfstdout,"%d\n", myMap<:m:>);
    }
  } 
}

void sig_main(exists map_t myMap <M:>){
  lpn_t firstB = {200, 306, 0000};
  lpn_t firstE = {201, 301, 0000};

  test(myMap, firstB, firstE);
}
