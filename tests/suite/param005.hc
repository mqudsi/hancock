/* Program tests parameterized function parameters to maps */
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

int f(char paramSet, int x, long long k, int *v){  
  int k2 = (int) k;
  if (paramSet)
    *v = x * k2;   /* x = 100, k2 = 1002, v = 100200 */
  else
    *v = k2 % 100; 
  return 0; /* no error */
}

map newMap_m (int x, foo_p c, int (* f1)(char paramSet, int x, long long,  int *) ) {
   key (x .. 999999LL);
   split (1000,10);
   value int;
   default f1(:x:);
   compress shrink(:c:);
   decompress expand(:c:);
  }

directory newDir_d(int x){
  foo_p c;
  newMap_m m(:x,c,f:);
}

int main(){
  int def = 100;
  newDir_d d(:def:) = "mapDir";
  int y;
  long long key = def * 10 + 2;  /* key = 1002 */
  y = d->m<:key:>;         /* first: y = 100200, second: y = 100201 */
  sfprintf(sfstdout, "The value of m<:%lld:> = %d.\n", key,y);
  y++;
  d->m<:key:> = y;         /* pickle value: first, 100201, second 100201 */
  return 0;
}
