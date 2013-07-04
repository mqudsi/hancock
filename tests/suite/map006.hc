#include "../scampRec.hh"

int shrink(int *from, unsigned char *to_space, int ext_to_size){
  *(int *)to_space = -(*from);
  return sizeof(*from);
};

int expand(unsigned char *from, int from_size, int *to_space){
  *to_space = -(*(int *)from);
  return sizeof(*to_space);
};

map cmap_t{
  key ppn_t;
  value int;
  default 0;
  compress shrink;
  decompress expand;
};

directory dir_t{
  cmap_t myMap;
  int myVal;
};


void sig_main(new dir_t myDir <d:>){
  lpn_t pn = {973, 360, 8675};

  myDir->myMap<:pn:> = 20;
  if(myDir->myMap@<:pn:>)
    sfprintf(sfstdout,"pn is in the map?\n");

  myDir->myMap\<:pn:>;
  if(myDir->myMap@<:pn:>)
    sfprintf(sfstdout,"Error: pn is still in the map?\n");
  else 
    sfprintf(sfstdout,"pn has been removed.\n");

  myDir->myVal = 20;
}
