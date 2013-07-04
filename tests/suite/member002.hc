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

void sig_main(exists map_t myMap <M:>){
  lpn_t firstB = {200, 305, 0000};

  sfprintf(sfstdout, "Second test.\n");
  query(myMap, firstB);
  myMap<:firstB:> = 10;
  query(myMap, firstB);
}






