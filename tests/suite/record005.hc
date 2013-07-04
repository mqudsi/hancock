view f(fS,fA) {
  int <=> char;

  fS(a) = a*2;
  fA(s) = s/2;
};

view g(gS,gA) {
  f x[2];
  f y;
  int z;
};

int main() {

  gS s = {{20,30},200,300};
  gA a;

  a = s$gA;
  sfprintf(sfstdout,"10 = %d, 15 = %d, 100 = %d, 300 = %d\n",a.x[0],a.x[1],a.y,a.z);

  s = a$gS;
  sfprintf(sfstdout,"20 = %d, 30 = %d, 200 = %d, 300 = %d\n",s.x[0],s.x[1],s.y,s.z);

  return 1;
}


