#include "scampRec.hh"				 /* ( 1) */
						 /* ( 2) */
map intlCount_m	{				 /* ( 3) */
  key (MINVALIDPN .. MAXVALIDPN-1);		 /* ( 4) */
  split	(10000,	1000);				 /* ( 5) */
  value	int;					 /* ( 6) */
  default 0;					 /* ( 7) */
};						 /* ( 8) */
						 /* ( 9) */
						 /* (10) */
void sig_main(callDetail_s callStream <c:>,	 /* (11) */
	      intlCount_m ic <I:>)		 /* (12) */
{						 /* (13) */
  int msgs;					 /* (14) */
						 /* (15) */
  iterate					 /* (16) */
    ( over callStream				 /* (17) */
      sortedby origin				 /* (18) */
      filteredby(c) (c->isIntl)			 /* (19) */
      withevents originDetect )	{		 /* (20) */
						 /* (21) */
    event line_begin(pn_t pn) {			 /* (22) */
      msgs = 0;					 /* (23) */
    }						 /* (24) */
						 /* (25) */
    /* saw an international call */		 /* (26) */
    event call(scampRec_t r) {			 /* (27) */
      msgs++;					 /* (28) */
    }						 /* (29) */
						 /* (30) */
    /* update the count for pn */		 /* (31) */
    event line_end(pn_t	pn) {			 /* (32) */
      ic<:pn:> = msgs +	ic<:pn:>;		 /* (33) */
    }						 /* (34) */
  };						 /* (35) */
}						 /* (36) */
