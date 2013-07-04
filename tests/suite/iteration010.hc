#define MAXKEY 99999999

map test_m{
  key (0 .. MAXKEY);
  split(1000, 100);
  value int;
  default 0;
}


int main(){
  test_m m = "../genData/data/testMap";
  m<:10:> = 100;
  iterate(over m)
  {
    event (long long * l) {
      sfprintf(sfstdout, "key value: %lld.\n", *l);
    }  
  }
  return 0;
}
