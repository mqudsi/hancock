#include "../scampRec.hh"

map map_t{
  key ppn_t;
  value int;
  default 2;
};

int indent = 0;
int indent_space = 4;
#define PR(A) (sfprintf(sfstdout,"%*s",indent,""), (A))


void test(map_t myMap, lpn_t start, lpn_t stop){
  PR(sfprintf(sfstdout,"start!\n"));
  indent+=indent_space;
  
  iterate(over myMap [start .. stop]
	  withevents lineDetect) {
    
    event npa_begin(areacode_t a){
      PR(sfprintf(sfstdout, "npa: %03d-%03d-%04d\n",a.npa,0,0));
      indent+=indent_space;
    }
    event nxx_begin(exchange_t n) {
      PR(sfprintf(sfstdout,"nxx: %03d-%03d-%04d\n",n.npa,n.nxx,0));
      indent += indent_space;
    }

    event line_begin(lpn_t m){
      PR(sfprintf(sfstdout,"(%03d)%03d-%04d: ",m.npa,m.nxx,m.line));
      sfprintf(sfstdout,"%*s",indent,"");
      sfprintf(sfstdout,"%d\n", myMap<:m:>);
    }

    event nxx_end() {
      indent -= indent_space;
    }
    event npa_end() {
      indent-=indent_space;
    }

  }

  indent-=indent_space;
  PR(sfprintf(sfstdout,"The End.\n"));

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
