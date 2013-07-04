#include "limits.hh"

#include "pht.hh"

typedef	struct {
   long	long origin;
   long	long dialed;
   unsigned int	poct;	    /* PHT_UNKNOWN if not in data. */
   unsigned int	soct;	    /* PHT_UNKNOWN if not in data. */
   unsigned int	pdct;	    /* PHT_UNKNOWN if not in data. */
   unsigned int	sdct;	    /* PHT_UNKNOWN if not in data. */
} wc_t;

int getValidCall(char paramSet,	pht_p p,
		 Sfio_t	*file, wc_t* lcr);

stream wireless_s(pht_p	p)
	   { getValidCall(: p :) : Sfio_t => wc_t; };

munion wline_e {: long long line_begin,
		  wc_t call,
		  long long line_end :}

wline_e	originDetect (wc_t *w[3:1]);
wline_e	dialedDetect (wc_t *w[3:1]);
