view f(fS,fA) {
  int <=> char;

  fS(a) = a*2;
  fA(s) = s/2;
};

view g(gS,gA) {
  f x;
  f y;
  int z;
};

int main() {

  gS s = {100,200,300};
  gA a;

  a = s$gA;
  sfprintf(sfstdout,"50 = %d, 100 = %d, 300 = %d\n",a.x,a.y,a.z);

  s = a$gS;
  sfprintf(sfstdout,"100 = %d, 200 = %d, 300 = %d\n",s.x,s.y,s.z);

  return 1;
}
