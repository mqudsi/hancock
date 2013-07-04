event line_e {: int npa, char nxx, long line :}
event cd_e {: int dur, line_e o :};

line_e setNpa(int x){
 return {:npa = x:};
} 

line_e setNxx(int x){
 return {:nxx = x:};
} 

line_e setLine(int x){
 return {:line = 3*x :};
} 

cd_e genDuration(int x){
 return {:dur = x:};
}

/*
cd_e genOrigin(int x){
 line_e l1;
 line_e l2;
 line_e l3;
 l1 = setNpa(973);
 l2 = setLine(x);
 l3 = l1 :+: l2;
 return {: o = l3  :};
}
*/

cd_e genOrigin(int x){
 line_e l1;
 line_e l2;
 line_e l3;
 line_e l4;

 l1 = setNpa(973);
 l2 = setNxx(973);
 l3 = setLine(x);
 l4 = l1 :+: l2 :+: l3; 
 return {: o = l4  :};
}

/*
cd_e genOrigin(int x){
 return {: o = (setNpa(973) :+: setLine(x))  :};
}
*/

void printE(char mem, char *s){
 if (mem)
  sfprintf(sfstdout, "%s event detected.\n", s);
 else 
  sfprintf(sfstdout, "no %s event detected.\n", s);
}

void test(line_e l){
  printE(l@npa, "npa");
  printE(l@nxx, "nxx");
  printE(l@line, "line");
}

void testCD(cd_e c){
  printE(c@o.npa, "o.npa");
  printE(c@o.nxx, "o.nxx");
  printE(c@o.line, "o.line");
  printE(c@dur, "duration");
}

void main(){
 cd_e c1;
 cd_e c2;
 c1 = genDuration(10);
 testCD(c1);
 c2 = genOrigin(3675);
 testCD(c2);
 testCD(c1:+:c2);
}




