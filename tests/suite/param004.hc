/* Program tests function parameters to maps */

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

void f(char paramSet, int x, long long k, int *v){  
  if (paramSet)
    *v = x;
  else
    *v = (int)k % 100; 
}

int f1(int x) {
  return x + 7;
}

map newMap_m (int x, foo_p c, int (* f1)(int) ) {
   key (x .. 999999LL);
   split (1000,10);
   value int;
   default f1(x);             /* if x = 100, default = 107 */
   compress shrink(:c:);
   decompress expand(:c:);
  }

directory newDir_d(int x){
  foo_p c;
  newMap_m m(:x,c,f1:);
}

int main(){
  int def = 100;
  newDir_d d(:def:) = "mapDir";
  int y;
  long long key = def * 10 + 2;  /* key = 1002 */
  y = d->m<:key:>;         /* first: y = 107, second: y = 108 */
  sfprintf(sfstdout, "The value of m<:%lld:> = %d.\n", key,y);
  y++;
  d->m<:key:> = y;         /* pickle value: first, 108, second 109 */
  return 0;
}
