#ifndef __AGE_HH
#define __AGE_HH

#define AGED       39
#define MAXACTIVE  39
#define MIN(x,y)   ( ((x) < (y))? (x) : (y) )

view aField(aFSig, aFApprox){
  int <=> char;
  aFSig(a) = a;
  aFApprox(s) =MIN(s, MAXACTIVE); 
};

view aLine(aLSig, aLApprox) {
  aField inA;
  aField out;
  aField jumble;
};

#define base (MAXACTIVE+1)

int Acompress_record(aLApprox *from, unsigned char *to_space, int ext_to_size)
{
   unsigned short v;

   v = from->jumble*base*base + from->inA*base + from->out;
   to_space[1] = (v >> 0x8) & 0xFF;

   to_space[0] = v & 0xFF;
   return 2;
	
}


int Auncompress_record(unsigned char *from, int from_size, aLApprox *to_space)
{ unsigned short v;

  v = (from[1] << 0x8) | from[0];
  to_space->jumble = v/(base*base);
  v = v%(base*base);
  to_space->inA = v/base;
  to_space->out = v % base;

  return 2;

}

/* void initAmap(aMap a)
{
  HRSsetRecordFn((HRSmap_t)a, 
        (int (*) (char *, int, char *, int)) Acompress_record, 
        (int (*) (char *, int, char *, int)) Auncompress_record);
}
*/

map aMap {
  key ppn_t;
  value aLApprox;
  default {MAXACTIVE,MAXACTIVE,MAXACTIVE};
  compress Acompress_record;
  decompress Auncompress_record;
};

#endif



