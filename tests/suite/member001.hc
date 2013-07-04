#include "../scampRec.hh"

map map_t{
  key ppn_t;
  value int;
  default 2;
};

void query(map_t myMap, lpn_t number){
  if (myMap@<:number:>) 
    sfprintf(sfstdout, "In map.\n");		
  else
    sfprintf(sfstdout, "Not in map.\n");		

}

void sig_main(new map_t myMap <M:>){
  lpn_t firstB = {200, 305, 0000};

  sfprintf(sfstdout, "First test.\n");
  query(myMap, firstB);
  myMap<:firstB:> = 10;
  query(myMap, firstB);
}






