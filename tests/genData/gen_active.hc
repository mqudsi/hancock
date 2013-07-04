#include "../scampRec.hh"
#include "../suite/active.hh"
#include <stdio.h>

lpn_t build_pn(int npa, int nxx, int line){
  lpn_t temp;
  temp.npa = npa;
  temp.nxx = nxx;
  temp.line = line;
  return temp;
}

void ch_map(aMap a)
{
  iterate(over a [build_pn(0,0,0)..build_pn(999,999,9999)]
	  withevents lineDetect) {
    
    event line_begin(lpn_t m) {
      aLApprox x;
      printf("%03d %03d %04d: ",m.npa, m.nxx, m.line);
      x = a<:m:>;
      printf("%2d %2d %2d\n", x.inA, x.out, x.jumble);

    }
  }
}

void sig_main() {
  new aMap a = "data/activeMap";
  new aMap b = "data/activeMapDC";

  lpn_t l = build_pn(973,360,8077);
  aLApprox d = {36,36,36};
  
  a<:l:> = d;
  b<:l:> = d;	

  // Check that the data got put.
  // aMaps contain three char buckets in the range 0 .. MAXACTIVE
  // called inA, out and jumble.
  printf("Checking activeMap.\n");
  ch_map(a);
  printf("Checking activeMapDC.\n");
  ch_map(b);
}
