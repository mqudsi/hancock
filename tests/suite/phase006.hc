#include "../scampRec.hh"

int indent = 0;
int indent_space = 2;
#define PR(A) (sfprintf(sfstdout,"%*s",indent,""), (A))


void print_scamp(HscampRec r) {
  PR(sfprintf(sfstdout,
            "calls: %03d-%03d-%04d(o) %03d-%03d-%04d(t) %03d-%03d-%04d(d) %d %d\n",
	    r.origin.npa,r.origin.nxx,r.origin.line,
	    r.terminated.npa,r.terminated.nxx,r.terminated.line,
	    r.dialed.npa,r.dialed.nxx,r.dialed.line,
            r.connecttime, r.duration)); 
}

int noHups(HscampRec *h){
  return h->duration;
}

void phaseBegin(){
    PR(sfprintf(sfstdout,"start!\n"));
    indent+=indent_space;
}

void phaseEnd(){
    indent-=indent_space;
    PR(sfprintf(sfstdout,"The End.\n"));
}

void areaCodeBegin(areacode_t a){
      PR(sfprintf(sfstdout, "npa: %03d-%03d-%04d\n",a.npa,0,0));
      indent+=indent_space;
}

void areaCodeEnd(areacode_t a){
      indent-=indent_space;
}


void exchangeCodeBegin(exchange_t n){
      PR(sfprintf(sfstdout,"nxx: %03d-%03d-%04d\n",n.npa,n.nxx,0));
      indent+=indent_space;
}

void exchangeCodeEnd(exchange_t n){
      indent-=indent_space;
}

void lineCodeBegin(lpn_t pn){
      PR(sfprintf(sfstdout,"line: %03d-%03d-%04d\n",pn.npa,pn.nxx,pn.line));
      indent+=indent_space;
}

void lineCodeEnd(lpn_t pn){
      indent-=indent_space;
}

void detect(cdStream calls){

  phaseBegin();
  
  iterate(over calls
	  sortedby origin, dialed, connecttime
	  filteredby noHups
	  withevents originDetect) {

    event npa_begin(areacode_t a){
      areaCodeBegin(a);
    }
    event nxx_begin(exchange_t n) {
      exchangeCodeBegin(n);
    }
    event line_begin(lpn_t pn) {
      lineCodeBegin(pn);
    }
 
    event call(HscampRec c) {
      char mytemp;
      print_scamp(c);
    }

    event line_end(lpn_t pn) {
      lineCodeEnd(pn);
    }
    event nxx_end(exchange_t n) {
      exchangeCodeEnd(n);
    }
    event npa_end(areacode_t a){
      areaCodeEnd(a);
    }
    
  }

  phaseEnd();
    
}

void sig_main(cdStream calls <o:>){
  sfprintf(sfstdout, "Input should be small!\n");
  detect(calls);
}


