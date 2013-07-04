#include	"vchdr.h"

/*	Functions private to the vcodex library.
**
**	Written by Kiem-Phong Vo (kpv@research.att.com)
*/

/* obtain an output buffer with proper size */
#if __STD_C
Void_t* _vcbuffer(Vcodex_t* vc, size_t size, int resize)
#else
Void_t* _vcbuffer(vc, size, resize)
Vcodex_t*	vc;
size_t		size;	/* required new size	*/
int		resize;	/* if this is resizing	*/
#endif
{
	if(size <= vc->size)
		return vc->buf;

	if(resize)
	{	if(!vc->buf || vc->size == 0)
			goto do_malloc;
		vc->size = (vc->buf = realloc(vc->buf,size)) ? size : 0;
	}
	else
	{ do_malloc:
		if(vc->buf && vc->size > 0)
			free(vc->buf);
		vc->size = (vc->buf = malloc(size)) ? size : 0;
	}

	return vc->buf;
}
