#include	"vchdr.h"

/*	Close a Vcodex_t handle
**
**	Written by Kiem-Phong Vo (kpv@research.att.com)
*/

#if __STD_C
int vcclose(Vcodex_t* vc)
#else
int vcclose(vc)
Vcodex_t*	vc;
#endif
{
	if(!vc)
		return -1;

	if(vc->disc && vc->disc->eventf &&
	   (*vc->disc->eventf)(vc, VC_CLOSING, NIL(Void_t*), vc->disc) < 0 )
		return -1;

	if(vc->meth && vc->meth->eventf &&
	   (*vc->meth->eventf)(vc, VC_CLOSING, NIL(Void_t*)) < 0 )
		return -1;

	if(vc->buf && vc->size > 0)
		free(vc->buf);

	free(vc);

	return 0;
}
