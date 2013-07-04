#include "../scampRec.hh"

void out(cdStream calls) {
  int my_pn2;
  
  iterate(
	  over calls
	  withevents originDetect) {
    event call() {
      static int x = 0;
      x++;
      sfprintf(sfstdout, "The value of x is: %d.\n", x);
    }
  }   
}

int sig_main(char * streamName <s:>){
  cdStream myStream = streamName;
  out(myStream);
  return 0;
}
