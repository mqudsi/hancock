#include "../scampRec.hh"

map map_t{
  key ppn_t;
  value int;
  default 2;
};


void test(map_t myMap, lpn_t start, lpn_t stop){
  iterate(over myMap [start .. stop]
	  withevents mapDetect) {
    
    event line_begin(lpn_t m){
      sfprintf(sfstdout,"(%03d)%03d-%04d: ",m.npa,m.nxx,m.line);
      sfprintf(sfstdout,"%d\n", myMap<:m:>);
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
