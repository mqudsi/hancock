#include "hl.hh"

#define MAXKEY 99999999
map test_m {
  key (0 .. MAXKEY);
  split(1000, 100);
  value int;
  default 0;
}


void fill(test_m m, long long start, long long end, int inc)
{
  long long i;

  for (i=start; i < end; i += inc)
    m<:i:> = 7;
}

HLmapStream_s mk(test_m m1, test_m m2) {
  return HLcreateMapMerge2(m1, m2, 1, 0, 0, OR_MERGE);
}


int sig_main()
{
  new test_m m1 = "../genData/data/hl.testMap.1";
  new test_m m2 = "../genData/data/hl.testMap.2";
  HLmapStream_s s;

  fill(m1, 1000LL, 1050, 3);
  fill(m2, 1001LL, 1050, 3);

  s = mk(m1, m2);
  iterate 
    ( over s ) {

    event (long long *k) {
      sfprintf(sfstdout, "%lld\n", *k);
    }
  }
}
