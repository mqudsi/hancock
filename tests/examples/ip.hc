#include "ipRec.hh"					     /* ( 1) */
#include "ihash.h"					     /* ( 2) */
							     /* ( 3) */
hash_table *ofInterest;					     /* ( 4) */
							     /* ( 5) */
int inSet(ipPacket_t *p)				     /* ( 6) */
{							     /* ( 7) */
   if (hash_get(ofInterest, p->source.hash_value) == 1)	     /* ( 8) */
      return 1;						     /* ( 9) */
							     /* (10) */
   if (hash_get(ofInterest, p->dest.hash_value)	== 1)	     /* (11) */
      return 1;						     /* (12) */
   return 0;						     /* (13) */
}							     /* (14) */
							     /* (15) */
void sig_main(ipAddr_s addrs <l:>,			     /* (16) */
	      ipPacket_s packets <p:>)			     /* (17) */
{							     /* (18) */
  /* code to set up hash table */			     /* (19) */
  ofInterest = hash_empty();				     /* (20) */
							     /* (21) */
  iterate						     /* (22) */
    ( over addrs ) {					     /* (23) */
							     /* (24) */
    event (ipAddr_t *addr) {				     /* (25) */
      if (hash_insert(ofInterest, addr->hash_value, 1) < 0)  /* (26) */
	 exit(1);					     /* (27) */
    }							     /* (28) */
  }							     /* (29) */
							     /* (30) */
  /* code to select packets */				     /* (31) */
  iterate						     /* (32) */
    ( over packets					     /* (33) */
      filteredby inSet ) {				     /* (34) */
							     /* (35) */
    event (ipPacket_t *p) {				     /* (36) */
      printPacketInfo(p);				     /* (37) */
    }							     /* (38) */
  };							     /* (39) */
}							     /* (40) */
