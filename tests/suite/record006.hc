view f(fS,fA) {
  int <=> char;

  fS(a) = a*2;
  fA(s) = s/2;
};

view g(gS,gA) {
  f x[2];
  fS y;
  int <=> char z;
  
  gS(a) {
    gS s;

    s.x[0] = a.x[0]/2;
    s.x[1] = a.x[1]/2;
    s.y = a.y;
    s.z = a.z + 200;

    return s;
  }

  gA(s) {
    gA a;

    a.x[0] = s.x[0]*2;
    a.x[1] = s.x[1]*2;
    a.y = s.y;
    a.z = s.z - 200;

    return a;
  }

};

int main() {

  gS s = {{20,30},200,300};
  gA a;

  a = s$gA;
  sfprintf(sfstdout,"40 = %d, 60 = %d, 200 = %d, 100 = %d\n",a.x[0],a.x[1],a.y,a.z);

  s = a$gS;
  sfprintf(sfstdout,"20 = %d, 30 = %d, 200 = %d, 300 = %d\n",s.x[0],s.x[1],s.y,s.z);

  return 1;
}
