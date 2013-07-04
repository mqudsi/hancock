/* Program map copy to imcomplete map with a parameter.  */
#include "pickle01.hh"
int mapNumber = 1;

int defFun(char paramSet, foo_p p, long long k, int *val){
  static int next = 1;
  sfprintf(sfstdout, "In defFun.\n");
  if (paramSet) {
    *val = next; 
    next++;
    sfprintf(sfstdout, "Param Set. MapNumber = %d, key = %lld, value = %d.\n", mapNumber, k, *val);
  } else {
    *val = next;
    next++;
    sfprintf(sfstdout, "Param not set. MapNumber = %d, key = %lld, value = %d.\n", mapNumber, k, *val);
  }
  return 0; /* no error */
}

map newMap_m (int x, foo_p c, int sp) {
   key (x .. 999999LL);
   split (1000,sp);
   value int;
   default defFun(:c:);
  }

munion base_e {: long long key :};
base_e baseDetect(long long *w[1:0]){
  return {: key = (*w[0]) :};
}

void init(newMap_m m){
  long long i;
  int temp;
  for(i=100; i<=105; i++) {
    m<:i:> = (int) i;
    temp = m<:i:>;
    sfprintf(sfstdout, "Stored for key %lld the value %d\n", i, temp);
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
  newMap_m m1(:def,c,sp:) = "newMap1";
  newMap_m m2 = "newMap2";
  sfprintf(sfstdout, "Key %d had value %d in map1\n", 1000, m1<:1000:>);

  init(m1);
  m2 :=: m1;
  sfprintf(sfstdout, "Copied map.\n");
  mapNumber = 2;
  sfprintf(sfstdout, "Key %d had value %d.\n", 200, m2<:200:>);
  readMap(m2);
  return 0;
}



