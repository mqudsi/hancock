#include	"vchdr.h"

/*	Map bytes from one to another (e.g., the rot13 program).
**
**	Written by Kiem-Phong Vo (kpv@research.att.com)
*/

typedef struct _vcmap_s
{	Vcchar_t	map[256];	/* the byte map	*/
} Vcmap_t;

#if __STD_C
static ssize_t vcmap(Vcodex_t* vc, const Void_t* data, size_t size,
			Void_t** out, size_t nout)
#else
static ssize_t vcmap(vc, data, size, out, nout)
Vcodex_t*	vc;
Void_t*		data;
size_t		size;
Void_t**	out;
size_t		n_out;
#endif
{
	Vcchar_t	*d, *endd, *output, *o;
	Vcchar_t	*map = ((Vcmap_t*)vc->mtdata)->map;

	if(!out || (nout > 0 && nout < size) )
		return size;
	else if(nout == 0)
	{	if(!(output = _vcbuffer(vc, size, 0)) )
			return -1;
		*out = output;
	}
	else	output = *out;

	for(o = output, endd = (d = (Vcchar_t*)data) + size; d < endd; )
		*o++ = map[*d++];

	return size;
}

#if __STD_C
static int vcmevent(Vcodex_t* vc, int type, Void_t* params)
#else
static int vcmevent(vc, type, params)
Vcodex_t*	vc;
int		type;
Void_t*		params;
#endif
{
	int		i;
	Vcchar_t	*m, *endm;
	Vcmap_t*	vcm;

	if(type == VC_OPENING)
	{	if(!(vcm = (Vcmap_t*)malloc(sizeof(Vcmap_t))) )
			return -1;

		vc->mtdata = (Void_t*)vcm;
		goto set_map;
	}
	else if(type == VC_DISC)
	{	if((vcm = vc->mtdata) )
		{ set_map:
			for(i = 0; i < 256; ++i)
				vcm->map[i] = i;
			if(vc->disc && (m = (Vcchar_t*)vc->disc->data) )
			{	for(endm = m + vc->disc->size; m+1 < endm; m += 2)
					vcm->map[m[0]] = m[1];
			}
		}
	}
	else if(type == VC_CLOSING)
	{	if(vc->mtdata)
			free(vc->mtdata);
		vc->mtdata = NIL(Void_t*);
	}

	return 0;
}

static Vcmethod_t _Vcmap =
{	vcmap,
	vcmevent,
	VC_MTMAP
};

Vcmethod_t*	Vcmap = &_Vcmap;
