#include "stream01.hh"

int indent = 0;
int indent_space = 2;
#define PR(A) (sfprintf(sfstdout,"%*s",indent,""), (A))

int notTF(HscampRec *hr) {
  return !(hr->isTollFree);
}


void print_scamp(HscampRec r) {
  PR(sfprintf(sfstdout,
            "calls: %03d-%03d-%04d(o) %03d-%03d-%04d(t) %03d-%03d-%04d(d) %d %d\n",
	    r.origin.npa,r.origin.nxx,r.origin.line,
	    r.terminated.npa,r.terminated.nxx,r.terminated.line,
	    r.dialed.npa,r.dialed.nxx,r.dialed.line,
            r.connecttime, r.duration)); 
}

void detect(cdStream calls) {
  PR(sfprintf(sfstdout,"start!\n"));
  indent+=indent_space;
  
  iterate(over calls 
	  filteredby notTF
	  sortedby dialed, origin, duration
	  withevents originDetect) {
    
    event npa_begin(areacode_t a){
      PR(sfprintf(sfstdout, "npa: %03d-%03d-%04d\n",a.npa,0,0));
      indent+=indent_space;
    }
    
    event nxx_begin(exchange_t n) {
      PR(sfprintf(sfstdout,"nxx: %03d-%03d-%04d\n",n.npa,n.nxx,0));
      indent += indent_space;
    }

    event line_begin(lpn_t pn) {
      PR(sfprintf(sfstdout,"line: %03d-%03d-%04d\n",pn.npa,pn.nxx,pn.line));
      indent+=indent_space;
    }

    event call(HscampRec c) {
      print_scamp(c);
    }

    event line_end() {
      indent-=indent_space;
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


int main() {
  cdStream calls(:5:) = "../genData/data/calls10";
  sfprintf(sfstdout, "Input should be small!\n");
  detect(calls);
  return 0;
}
