view f(fS,fA) {
  int <=> char;
  fS(a) = a*2;
  fA(s) = s/2;
};

int main() {
 fS s = 10;
 fA a;

 a = s$fA;
 sfprintf(sfstdout, "5 = %d ?\n",a);
 s = a$fS;
 sfprintf(sfstdout, "10 = %d \n",s);

 return 1;
}
