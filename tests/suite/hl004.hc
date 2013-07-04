#include "hl.hh"

int sig_main()
{
  HLkey_s ks = "sfstdin";

  iterate 
    ( over ks ) {

    event (long long *k) {
      sfprintf(sfstdout, "%lld\n", *k);
    }
  }
}
