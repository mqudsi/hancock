#include "../scampRec.hh"

void test(cdStream calls){

  int count = 0;

  iterate(over calls
	  sortedby dialed, duration
	  withevents originDetect) {
    
    event line_begin(lpn_t pn) {
      count = 0;
      sfprintf(sfstdout,"(%03d)%03d-%04d\n",pn.npa,pn.nxx,pn.line);
    }
      
    event call() {
      count++;
    }

    event line_end() {  
      sfprintf(sfstdout,"Count: %d.\n",count);
    }
    
  }
}

sig_main(const cdStream calls <o:>){
  sfprintf(sfstdout,"Start.\n");
  test(calls);
  sfprintf(sfstdout,"Finish.\n");
}
