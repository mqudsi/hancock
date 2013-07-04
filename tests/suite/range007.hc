#include "../scampRec.hh"

map aMap{
  key ppn_t;
  value int;
  default 9;
};

void writePPN(ppn_t pn){
  sfprintf(sfstdout, "\tThe value for (%d)%d-%d is: ", 
                     pn.primary / 1000, pn.primary % 1000, pn.secondary);
}

void writeLPN(lpn_t pn){
  sfprintf(sfstdout, "\tThe value for (%d)%d-%d is: ", 
                     pn.npa, pn.nxx, pn.line);
}

void readEntry(aMap m, lpn_t pn){
  int a;
  a = m<:pn:>; 
  sfprintf(sfstdout, "ReadEntry:\n");
  writeLPN(pn);
  sfprintf(sfstdout, "value: %d.\n", a);
}

void writeEntry(aMap m, lpn_t pn){
  int a;
  a = m<:pn:>; 
  a -= 1; 
  m<:pn:> = a;
  a = m<:pn:>;
  sfprintf(sfstdout, "writeEntry:\n");
  writeLPN(pn);
  sfprintf(sfstdout, "value: %d.\n", a);
}

int sig_main(new aMap test <v:>){
  ppn_t pn;
  lpn_t ln;
  pn.primary = 973360;
  pn.secondary = 8077;
  ln = pn$lpn_t;
  readEntry(test, ln);
  writeEntry(test, ln);
  return 1;
}











