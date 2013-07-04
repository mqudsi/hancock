#include "../scampRec.hh"
#include "constants.h"

typedef struct {
  int x,y,z;
} h;

h map_default = {1,2,3};

map aMap {
  key ppn_t;
  value h;
  default {1,2,3};
};

void sig_main(new aMap x<a:>)
{
  ppn_t pn1 = {650472,1577};
  h g;;

  g = x<:pn1:>;

  sfprintf(sfstdout, "{%d,%d,%d} = {%d,%d,%d} ? \n",g.x,g.y,g.z,
  	   map_default.x,map_default.y,map_default.z);

}
