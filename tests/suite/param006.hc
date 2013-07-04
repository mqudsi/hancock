/* Program tests iteration over parameterized map.  */
#include "pickle01.hh"

int shrink(char paramsSupplied, foo_p c, int *from, unsigned char *to_space, int ext_to_size){
  if (paramsSupplied) c->val = *from;  /* Record last compressed value in pickle. */
  *(int *)to_space = -(*from);
  return sizeof(*from);
};

int expand(char paramsSupplied, foo_p c, unsigned char *from, int from_size, int *to_space){
  *to_space = -(*(int *)from);
  if (paramsSupplied) {
    if (c->val = *to_space)
      sfprintf(sfstdout, "Pickle value matched last compressed value: %d.\n", *to_space);
    else
      sfprintf(sfstdout, "Pickle value = %d; last compressed value: %d.\n", c->val, *to_space);
  }
  return sizeof(*to_space);
};

map newMap_m (int x, foo_p c, int sp) {
   key (x .. 999999LL);
   split (1000,sp);
   value int;
   default x;
   compress shrink(:c:);
   decompress expand(:c:);
  }

munion base_e {: long long key :};

base_e baseDetect(long long *w[1:0]){
  return {: key = (*w[0]) :};
}

void init(newMap_m m){
  long long i;
  int temp;
  for(i=100; i<=110; i++) {
    m<:i:> = (int)i;
    temp = m<:i:>;
    sfprintf(sfstdout, "Stored for key %d the value %d\n", (int)i, temp);
  }
}
void readMap(newMap_m m) {
  iterate (over m[100LL .. 110LL]
           withevents baseDetect) {
    event key(long long k){
      sfprintf(sfstdout, "Key %lld had value %d\n", k, m<:k:>);
    }
  }
}
int main(){
  int def = 100;
  int sp = 10;
  foo_p c = "cacheP";
  newMap_m m(:def,c,sp:) = "newMap";
  init(m);
  readMap(m);
  return 0;
}



