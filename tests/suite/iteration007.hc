#include "../scampRec.hh"

int indent = 0;
int indent_space = 4;
#define PR(A) (sfprintf(sfstdout,"%*s",indent,""), (A))

map map_t{
  key ppn_t;
  value int;
  default 2;
};

line_e pMapDetect(ppn_t *w[1:0]) {
  return {: line_begin = (*w[0])$lpn_t :};
}


int noEven(ppn_t * pn){
   lpn_t lpn;
   lpn = (*pn)$lpn_t;
   return lpn.line % 2;
}

void test(map_t myMap, ppn_t start, ppn_t stop){
  iterate(over myMap [start .. stop]
          filteredby noEven
          sortedby secondary
	  withevents pMapDetect) {
    
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

  test(myMap, firstB$ppn_t, firstE$ppn_t);
}
