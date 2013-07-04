#include	"vchdr.h"

/*	Change the discipline for a Vcodex_t handle.
**
**	Written by Kiem-Phong Vo (kpv@research.att.com)
*/

#if __STD_C
Vcdisc_t* vcdisc(Vcodex_t* vc, Vcdisc_t* disc)
#else
Vcdisc_t* vcdisc(vc, disc)
Vcodex_t*	vc;
Vcdisc_t*	disc;
#endif
{
	Vcdisc_t*	old;

	if(!vc || !disc)
		return NIL(Vcdisc_t*);

	old = vc->disc;

	/* see if method is agreeable to changing discipline at this time */
	if(vc->meth && vc->meth->eventf &&
	   (*vc->meth->eventf)(vc, VC_DISC, (Void_t*)disc) < 0 )
	{	if(old && old->eventf)
			(void)(*old->eventf)(vc, VC_ERRINIT, vc->meth, old);
		return NIL(Vcdisc_t*);
	}

	/* now check with incumbent to see if it's ok with being changed */
	if(old && old->eventf &&
	   (*old->eventf)(vc, VC_DISC, (Void_t*)disc, old) < 0 )
		return NIL(Vcdisc_t*);

	vc->disc = disc;
	return old;
}
