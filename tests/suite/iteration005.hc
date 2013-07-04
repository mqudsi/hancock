#include "../scampRec.hh"

map map_t{
  key ppn_t;
  value int;
  default 2;
};

lpn_t build_pn(int npa, int nxx, int line){
  lpn_t temp;
  temp.npa = npa;
  temp.nxx = nxx;
  temp.line = line;
  return temp;
}

void do_exchange(map_t myMap, int npa, int nxx){
  iterate(over myMap [build_pn(npa,nxx,0) .. build_pn(npa,nxx,9999)]
	  withevents mapDetect) {

    event line_begin(lpn_t m){
      sfprintf(sfstdout,"%04d: ",m.line);
      sfprintf(sfstdout,"%d\n", myMap<:m:>);
    }
  }
}

void do_areacode(map_t myMap, int npa){
  iterate(over myMap [build_pn(npa,0,0) .. build_pn(npa,999,9999)]
	  withevents lineDetect) {
    
    event nxx_begin(exchange_t exn){
      sfprintf(sfstdout,"(%03d)%03d-%04d:\n",exn.npa,exn.nxx,0);
      do_exchange(myMap, exn.npa,exn.nxx);
    }
  }
}

void sig_main(exists map_t myMap <M:>){
  sfprintf(sfstdout, "First test.\n");
  do_areacode(myMap, 200);
  sfprintf(sfstdout, "Second test.\n");
  do_areacode(myMap, 201);
}
