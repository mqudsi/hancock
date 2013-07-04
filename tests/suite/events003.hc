
event line_e {: int npa, char nxx, long line :}

event cd_e {: int duration, line_e o, line_e d, line_e t, int connect :};

line_e gen(int x){
 return {:npa = x, line = 3*x :};
} 

cd_e genCD(int x){
 return {:duration = x, o = gen(2*x) :};
}

void printE(char mem, char *s){
 if (mem)
  sfprintf(sfstdout, "%s event detected.\n", s);
 else 
  sfprintf(sfstdout, "no %s event detected.\n", s);
}

void test(line_e l){
  printE(l@npa, "npa");
  printE(l@nxx, "nxx");
}

void testCD(cd_e c){
  printE(c@connect, "connect");
  printE(c@o.npa, "npa");
  printE(c@o.nxx, "nxx");
  printE(c@d.npa, "npa");
  printE(c@d.nxx, "nxx");
  printE(c@duration, "duration");
}


      
void main(){
 line_e l;
 cd_e c;
 l = gen(3);
 test(l);
 l = (line_e){: :};
 test(l);
 c = genCD(4);
 testCD(c);
}




