/* Program tests initializing decls for maps with no params supplied.  */
int oshrink(int *from, unsigned char *to_space, int ext_to_size){
  *(int *)to_space = -(*from);
  return sizeof(*from);
};

int oexpand(unsigned char *from, int from_size, int *to_space){
  *to_space = -(*(int *)from);
  return sizeof(*to_space);
};

map newMap_m (int x, long long y, int bp, int sp) {
   key (x .. y);
   split (bp,sp);
   value int;
   default x;
   compress oshrink;
   decompress oexpand;
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
  int upper = 9999999;
  int sp = 10;
  int bp = 10000;
  newMap_m m = "newMap";  /* map generated my param007.exe */
  readMap(m);
  return 0;
}



