#include	"vchdr.h"

static char*	Version = "\n@(#)vcodex (AT&T Labs - kpv) 2000-09-01\0\n";

/*	Open a handle for data coding
**
**	Written by Kiem-Phong Vo (kpv@research.att.com)
*/

static Vcdisc_t	_Nildisc;	/* the empty discipline */

#if __STD_C
Vcodex_t* vcopen(Vcdisc_t* disc, Vcmethod_t* meth, Void_t* init, int flags)
#else
Vcodex_t* vcopen(disc, meth, init, flags)
Vcdisc_t*	disc;
Vcmethod_t*	meth;
Void_t*		init;
int		flags;
#endif
{
	Vcodex_t*	vc = (Vcodex_t*)Version; /* stop compiler warning */

	if(!meth || !(vc = (Vcodex_t*)malloc(sizeof(Vcodex_t))) )
		return NIL(Vcodex_t*);

	vc->disc = disc ? disc : &_Nildisc;
	vc->meth = meth;
	vc->flags = flags & VC_FLAGS;
	vc->buf = NIL(Void_t*);
	vc->size = 0;
	vc->mtdata = NIL(Void_t*);

	if(disc->eventf && (*disc->eventf)(vc, VC_OPENING, NIL(Void_t*), disc) < 0)
	{	free(vc);
		return NIL(Vcodex_t*);
	}

	if(meth->eventf && (*meth->eventf)(vc, VC_OPENING, init) < 0)
	{	if(disc->eventf)
			(void)(*disc->eventf)(vc, VC_ERRINIT, meth, disc);
		free(vc);
		return NIL(Vcodex_t*);
	}

	return vc;
}
