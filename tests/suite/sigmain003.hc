#include "../scampRec.hh"


void chStream(cdStream s, char * name){
   sfprintf(sfstdout, "%s stream is ", name);
   if (s == NULL)
      sfprintf(sfstdout, "null.\n");
   else
      sfprintf(sfstdout, "non-null.\n");
}

int sig_main(const cdStream first default NULL  <f:>,
                   cdStream second default first <s:>,
             const cdStream third <t:>,
                   cdStream fourth <u:>){

  chStream(first, "first");
  chStream(second, "second");
  chStream(third, "third");
  chStream(fourth, "fourth");
}











