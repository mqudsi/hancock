range p_r = [0 .. 9];
range s_r   = [0 .. 9];

typedef struct {
  p_r p;
  s_r s;
} hkey_t;

map test_m{
  key hkey_t;
  value char;
  default 0;
}

munion always_e {: hkey_t click :};

always_e do_click(hkey_t *r[1:0]) {
  return {: click= *r[0] :};
}

int fillMap(test_m myMap){
  char i;
  hkey_t index = {0,0};
  for (i=0; i<9; i++){
    index.p = i; 
    index.s = i;
    myMap<:index:> = (char)('a' + i);
  }
}

int goo(test_m myMap){
  hkey_t start = {0,0};
  hkey_t stop = {9,9};

  iterate(over myMap[start..stop]
	  withevents do_click) {
    event click(hkey_t k){
      char c;
      c = myMap<:k:>;
      sfprintf(sfstdout,"%c\n",c);
      if (c == 'e') return 0;
    }
  }
  sfprintf(sfstdout, "\n");
  return 1;
}

void sig_main(new test_m myMap <m:>){
  int x;
  fillMap(myMap);
  x = goo(myMap);
  sfprintf(sfstdout, "x is : %d.\n", x);
}




