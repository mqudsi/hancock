#include "../scampRec.hh"

typedef struct{
  int8 c;
  uint8 uc;
  int16 s;
  uint16 us;
  int32 i;
  uint32 ui;
  int64 l;
  uint64 ul;
  float32 f;
  float64 d;
  int32 buf[5];
} port_s;

map portable_m {
  key ppn_t;
  value port_s;
  default {0,0,0,0,
           0,0,0,0,
           0.0, 0.0,
           {0,0,0,0,0}
             };
};

void printData(portable_m myMap, lpn_t pn){
  port_s myPort = myMap<:pn:>;
  int i;
  sfprintf(sfstdout, "Map value for (%d)%d-%d:\n", pn.npa, pn.nxx, pn.line);
  sfprintf(sfstdout, "c: %d\n",  myPort.c);
  sfprintf(sfstdout, "uc: %u \n",  myPort.uc);
  sfprintf(sfstdout, "s: %d \n",  myPort.s);
  sfprintf(sfstdout, "us: %u \n",  myPort.us);
  sfprintf(sfstdout, "i: %d \n",  myPort.i);
  sfprintf(sfstdout, "ui: %u \n", myPort.ui);
  sfprintf(sfstdout, "l: %ld\n", myPort.l);
  sfprintf(sfstdout, "ul: %lu \n", myPort.ul);
  sfprintf(sfstdout, "f: %f  d: %lf \n", myPort.f, myPort.d);
  sfprintf(sfstdout, "Array:\n");
  for(i=0; i<5; i++)
      sfprintf(sfstdout, "buf[%d] = %d\n", i, myPort.buf[i]);
}

int sig_main(portable_m myMap <m:, "map data">){
  lpn_t pn = {973, 984, 3167};
  port_s s;
  int i;
  printData(myMap, pn);
  s = myMap<:pn:>;
  s.c += 2;
  s.uc += 3;
  s.s += 4;
  s.us += 5;
  s.i += 6;
  s.ui += 7;
  s.l += 8;
  s.ul += 9;
  s.f += 4.0;
  s.d += 5.0;
  for(i=0; i<5; i++)
      s.buf[i] += i*10;
  myMap<:pn:> = s;
  printData(myMap, pn);
  return 0;
}
