view f(fS,fA) {
  int <=> char x;
  float <=> int y[10];

  fS(a) { 
    fS s;
    int i;	

    s.x = a.x*2;
    
    for(i=0;i<10;i++)
      s.y[i]=a.y[i]*5;

    return s;
  }

  fA(s) {
    fA a;
    int i=0;
    a.x = s.x/2;

    for(i=0;i<10;i++)
      a.y[i]=s.y[i]/5;

    return a;
  }

};

int main() {
 fS s = {10,{1,2,3,4,5,6,7,8,9,10}};
 fA a;
 int i;

 a = s$fA;
 sfprintf(sfstdout,"5 = %d and ",a.x);
 for(i=0;i<10;i++)
   sfprintf(sfstdout,"%d ",a.y[i]);
 sfprintf(sfstdout,"\n");

 s = a$fS;
 sfprintf(sfstdout,"10 = %d and ",s.x);
 for(i=0;i<10;i++)
   sfprintf(sfstdout,"%f ",s.y[i]);
 sfprintf(sfstdout,"\n");

 return 1;
}
