
event line_e {: int npa, char nxx, long line :}

event cd_e {: int duration, line_e o, line_e d, line_e t, int connect :};

line_e setNpa(int x){
 return {:npa = x:};
} 

line_e setLine(int x){
 return {:line = 3*x :};
} 

void test(line_e l){
  if(l@npa)  sfprintf(sfstdout,"npa = %d, ",l.npa);
  if(l@nxx)  sfprintf(sfstdout,"nxx = %d, ",l.nxx);
  if(l@line) sfprintf(sfstdout,"line = %d ",l.line);
  sfprintf(sfstdout,"\n");
}

void testArrow(line_e *l,line_e *l2) {
  (*l).npa = 200;
  l->nxx = 100;
  l->line = l2->line;
}

void testDiff() {
  line_e l1;
  line_e l2;

  l1.npa = 100;
  l1.nxx = 101;
  l1.line = 102;

  l2 = (line_e){::};
  l2.npa = 100;

  test(l1);
  test(l2);
  test(l1 :-: l2);

}      
void main(){
 line_e l1;
 line_e l2;
 line_e l3;

 l1 = setNpa(973);
 test(l1);
 l2 = setLine(8675);
 test(l2);
 l3 = l1 :+: l2;
 test(l3);

 l1 = (line_e){::}; // Clear it.
 testArrow(&l1,&l2);
 test(l1);

 testDiff();

}




