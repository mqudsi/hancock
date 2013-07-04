#include "scampRec.hh"					   /* ( 1) */
#include <stdio.h>					   /* ( 2) */
							   /* ( 3) */
void sig_main(callDetail_s callStream <c:>)		   /* ( 4) */
{							   /* ( 5) */
  int msgs = 0;						   /* ( 6) */
							   /* ( 7) */
  iterate						   /* ( 8) */
    ( over callStream					   /* ( 9) */
      filteredby(c) (c->isIntl)	) {			   /* (10) */
							   /* (11) */
    event (scampRec_t *c) {				   /* (12) */
      msgs++;		 /* saw an international call  */  /* (13) */
    }							   /* (14) */
  };							   /* (15) */
							   /* (16) */
  printf("There were %d international calls.\n", msgs);	   /* (17) */
}							   /* (18) */
