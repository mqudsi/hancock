#include "../scampRec.hh"

typedef struct{
  npanxx_r primary;
  line_r   second;
} p_line;

map aMap{
  key p_line;
  value int;
  default 9;
};

void writePN(p_line pn){
  sfprintf(sfstdout, "\tThe value for (%d)%d-%d is: ", 
                     pn.primary / 1000, pn.primary % 1000, pn.second);
}

void readEntry(aMap m, p_line pn){
  int a;
  a = m<:pn:>; 
  sfprintf(sfstdout, "ReadEntry:\n");
  writePN(pn);
  sfprintf(sfstdout, "value: %d.\n", a);
}

void writeEntry(aMap m, p_line pn){
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

  p_line pn;
  pn.primary = 973360;
  pn.second = 8077;
  readEntry(test, pn);
  writeEntry(test, pn);
  return 1;
}











