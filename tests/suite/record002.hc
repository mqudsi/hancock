view f(fS,fA) {
  int <=> char x;

  fS(a) { 
   fS s;
   s.x = a.x*2;
   return s;
  }

  fA(s) {
    fA a;
    a.x = s.x/2;
    return a;
  }

};

int main() {
 fS s = {10};
 fA a;

 a = s$fA;
 sfprintf(sfstdout, "5 = %d ?\n",a.x);
 s = a$fS;
 sfprintf(sfstdout, "10 = %d \n",s.x);
 
 return 1;
}
