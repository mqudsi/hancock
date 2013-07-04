#include "../scampRec.hh"

map aMap{
  key ppn_t;
  value int;
  default 9;
};

void writePN(ppn_t pn){
  sfprintf(sfstdout, "\tThe value for (%d)%d-%d is: ", 
                     pn.primary / 1000, pn.primary % 1000, pn.secondary);
}

void readEntry(aMap m, ppn_t pn){
  int a;
  a = m<:pn:>; 
  sfprintf(sfstdout, "ReadEntry:\n");
  writePN(pn);
  sfprintf(sfstdout, "value: %d.\n", a);
}

void writeEntry(aMap m, ppn_t pn){
  int a;
  a = m<:pn:>; 
  a -= 1; 
  m<:pn:> = a;
  a = m<:pn:>;
  sfprintf(sfstdout, "writeEntry:\n");
  writePN(pn);
  sfprintf(sfstdout, "value: %d.\n", a);
}

int sig_main(new aMap test <v:>){
  ppn_t pn;
  pn.primary = 973360;
  pn.secondary = 8077;
  readEntry(test, pn);
  writeEntry(test, pn);
  return 1;
}











