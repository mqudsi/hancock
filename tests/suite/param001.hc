#include "pickle01.hh"

void chkPickle(foo_p myPickle, char *which){
  sfprintf(sfstdout, "Initial pickle value for %s: %d\n", which, myPickle->val);
  myPickle->val = myPickle->val + 7;
  sfprintf(sfstdout, "New pickle value for %s: %d\n", which, myPickle->val);
}

int sig_main (int   i    <i:,"Initial value for pickle.">,
              foo_p myPickle1(:i:) <p:>,
	      char * str <s:,"Name of pickle file.">){
  foo_p myPickle2 = str; 
  chkPickle(myPickle1, "Pickle1");
  myPickle2 :=: myPickle1;
  chkPickle(myPickle2, "Pickle2");
  return 0;
}


