#ifndef _VCHDR_H
#define _VCHDR_H	1

#include	<ast_common.h>

#define _VCODEX_PRIVATE \
	int	flags;	/* currently unused	*/ \
	Void_t*	buf;	/* output buffer	*/ \
	size_t	size;	/* output buffer size	*/ \
	Void_t*	mtdata;	/* method data		*/

#include	"vcodex.h"

#ifdef DEBUG
_BEGIN_EXTERNS_
extern int		abort();
_END_EXTERNS_
#define ASSERT(p)	((p) ? 0 : abort())
#define COUNT(n)	((n) += 1)
#define TALLY(c,n,v)	((c) ? ((n) += (v)) : (n))
#define DECLARE(t,v)	t v
#define SET(n,v)	((n) = (v))
#else
#define ASSERT(p)
#define COUNT(n)
#define TALLY(c,n,v)
#define DECLARE(t,v)
#define SET(n,v)
#endif

#ifndef NIL
#define NIL(type)	((type)0)
#endif

#define VCSIZEU(v)	((v) < (1<<7) ? 1 : (v) < (1<<14) ? 2 : (v) < (1<<21) ? 3 : 4)
#define VCSIZEM(v)	((v) < (1<<8) ? 1 : (v) < (1<<16) ? 2 : (v) < (1<<24) ? 3 : 4)

_BEGIN_EXTERNS_
extern Void_t*		_vcbuffer _ARG_((Vcodex_t*, size_t, int));

extern Void_t*		memcpy _ARG_((Void_t*, const Void_t*, size_t));
extern Void_t*		malloc _ARG_((size_t));
extern Void_t*		realloc _ARG_((Void_t*, size_t));
extern Void_t*		calloc _ARG_((size_t, size_t));
extern void		free _ARG_((Void_t*));
_END_EXTERNS_

#endif /*_VCHDR_H*/
