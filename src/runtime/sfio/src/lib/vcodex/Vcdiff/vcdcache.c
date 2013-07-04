#include	"vcdhdr.h"

/*	Functions to encode/decode COPY addresses based on the caches.
**
**	Written by Kiem-Phong Vo (kpv@research.att.com)
*/

#if __STD_C
void vcdkaclose(Vcdcache_t* ka)
#else
void vcdkaclose(ka)
Vcdcache_t*	ka;
#endif
{
	if(ka)
	{	if(ka->same)
			free(ka->same);
		if(ka->near)
			free(ka->near);
		free(ka);
	}
}

/* initialize address caches */
#if __STD_C
Vcdcache_t*  vcdkaopen(int s_near, int s_same)
#else
Vcdcache_t*  vcdkaopen(s_near, s_same)
int		s_near;
int		s_same;
#endif
{
	Vcdcache_t*	ka;

	if(!(ka = (Vcdcache_t*)malloc(sizeof(Vcdcache_t))) )
		return NIL(Vcdcache_t*);
	ka->same = ka->near = NIL(int*);

	if((ka->s_near = s_near) > 0 &&
	   !(ka->near = (int*)malloc(s_near*sizeof(int))) )
	{	vcdkaclose(ka);
		return NIL(Vcdcache_t*);
	}

	if((ka->s_same = s_same) > 0 &&
	   !(ka->same = (int*)malloc(s_same*256*sizeof(int))) )
	{	vcdkaclose(ka);
		return NIL(Vcdcache_t*);
	}

	return ka;
}

#if __STD_C
void vcdkaclear(Vcdcache_t* ka)
#else
void vcdkaclear(ka)
Vcdcache_t*	ka;
#endif
{
	int	i;

	for(i = 0; i < ka->s_near; ++i)
		ka->near[i] = 0;
	ka->n = 0;

	for(i = 0; i < ka->s_same*256; ++i)
		ka->same[i] = 0;
}

/* update address caches */
#if __STD_C
static void vcdkaupdate(Vcdcache_t* ka, int addr)
#else
static void vcdkaupdate(ka, addr)
Vcdcache_t*	ka;
int		addr;
#endif
{
	if(ka->s_near > 0)
	{	ka->near[ka->n] = addr;
		if((ka->n += 1) >= ka->s_near)
			ka->n = 0;
	}

	if(ka->s_same > 0)
		ka->same[addr % (ka->s_same*256)] = addr;
}

/* compute encoding for COPY addresses */
#if __STD_C
int vcdkasetaddr(Vcdcache_t* ka, int addr, int here, int* mode)
#else
int vcdkasetaddr(ka, addr, here, mode)
Vcdcache_t*	ka;
int		addr;	/* matching address to be encoded	*/
int		here;	/* current location			*/
int*		mode;	/* to return the coded address		*/
#endif
{
	int	i, d, sz, bestd, bestm, bestsz;

	bestd = addr;
	bestm = VCD_SELF;
	if((bestsz = VCSIZEU(bestd)) == 1)
		goto done;

	d = here-addr;
	if((sz = VCSIZEU(d)) < bestsz)
	{	bestd = d;
		bestm = VCD_HERE;
		if((bestsz = sz) == 1)
			goto done;
	}

	for(i = 0; i < ka->s_near; ++i)
	{	if((d = addr - ka->near[i]) < 0)
			continue;
		if((sz = VCSIZEU(d)) < bestsz)
		{	bestd = d;
			bestm = (VCD_HERE+1) + i;
			if((bestsz = sz) == 1)
				goto done;
		}
	}

	if(ka->s_same > 0 && ka->same[d = addr%(ka->s_same*256)] == addr)
	{	bestd = d%256;
		bestm = (VCD_HERE+1) + ka->s_near + d/256;
	}

done:	vcdkaupdate(ka, addr);
	*mode = bestm;
	return bestd;
}


#if __STD_C
int vcdkagetaddr(Vcdcache_t* ka, Vcchar_t** addr, int here, int mode)
#else
int vcdkagetaddr(ka, addr, here, mode)
Vcdcache_t*	ka;
Vcchar_t**	addr;
int		here;
int		mode;
#endif
{
	int	a, m;

	if(mode == VCD_SELF)
		a = vcgetu(addr);
	else if(mode == VCD_HERE)
		a = here - vcgetu(addr);
	else if((m = mode - (VCD_HERE+1)) >= 0 && m < ka->s_near)
		a = ka->near[m] + vcgetu(addr);
	else if((m = mode - (VCD_HERE+1+ka->s_near)) >= 0 && m < ka->s_same)
		a = ka->same[vcgetc(addr) + m*256];
	else	return -1;

	vcdkaupdate(ka, a);

	return a;
}
