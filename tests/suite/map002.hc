#include "../scampRec.hh"
#include "constants.h"

map aMap {
  key ppn_t;
  value float;
};

void sig_main(new aMap x<a:>)
{
  float g=1.2345;
  ppn_t pn1 = {650472,1577};
  x<:pn1:> = g;
  g = x<:pn1:>;
	
  sfprintf(sfstdout, "g = %f should be 1.2345\n",g);

}
