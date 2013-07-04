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

void fill_it(map_t myMap) {
  int i,j,k;

  for(i=200;i<202;i++) {
    for(j=300;j<350;j+=10) {
      for(k=1000;k<1050;k+=15) {
	lpn_t x;
	x.npa = i;
	x.nxx = j;
	x.line = k;
	myMap<:x:> = k;
      }
    }
  }
}
void sig_main(new map_t myMap <M:>){
  line_t start = {200, 306, 0000};
  line_t stop =  {201, 301, 0000};
  int i;

  fill_it(myMap);
  test(myMap, start, stop);

}
