#include	"vchdr.h"

/*	Run length encoding.
**
**	Written by Kiem-Phong Vo (kpv@research.att.com)
*/

/* the schema for the bits of a control byte look like this:
**		c B r S
**	c: this is the continuation bit for integer encoding.
**	B: this could take from 0 to RL_MAXB bits to encode a byte
**		being specially coded in a rl-encoding. In that case,
**		the B-bits has a non-zero bit and the left-over rS bits
**		are usable for integer encoding of the length.
**	r: when B-bits are zero, this bit indicates whether the next byte
**		was rl-encoded.
**	S: when B-bits are zero, this is used to encode the length of either
**		the run-length sequence or the ADD bytes.
*/
#define RL_MAXB		3		/* max B for specially coded bytes	*/
#define RL_S(B)		(6 - (B))	/* # of S bits for length coding	*/
#define RL_r(B)		(1 << RL_S(B))	/* the r-bit given a B value		*/

/* value of B given the number of bytes to be specially coded */
#define RL_B(x)		(x >= 4 ? 3 : x >= 2 ? 2 : x >= 1 ? 1 : 0)

#define ALLOCINC	(32*1024)
#define ALLOCSZ(x)	((((x) + ALLOCINC - 1)/ALLOCINC)*ALLOCINC)
#define ALLOCMORE(x)	((x) <  64*1024 ?  32*1024 : (x) < 128*1024 ?  64*1024 : \
			 (x) < 256*1024 ? 128*1024 : (x) < 512*1024 ? 256*1024 : \
			 (x) < 512*1024 ? 256*1024 : 512*1024 )

#ifdef DEBUG
#define WRADD(fd,n) \
		{char tr[64]; sprintf(tr,"add %4d\n",n); write(fd,tr,strlen(tr));}
#define WRRUN(fd,n,b) \
		{char tr[64]; sprintf(tr,"run %4d %u\n",n,b); write(fd,tr,strlen(tr));}
#else
#define WRADD(fd,n)
#define WRRUN(fd,n,b)
#endif

typedef struct _freq_s
{	size_t		freq;	/* frequency of the below byte	*/
	Vcchar_t	byte;
} Freq_t;

typedef struct _vcrle_s
{	int		learn;		/* 1: if learned	*/
	Freq_t		freq[256];	/* byte frequencies	*/
	int		nbyte;		/* # of special bytes	*/
	Vcchar_t	byte[1 << RL_MAXB]; /* special bytes 	*/
} Vcrle_t;

#if __STD_C
static int freqcmp(const Void_t* one, const Void_t* two)
#else
static int freqcmp(one, two)
Void_t*		one;
Void_t*		two;
#endif
{
	/* sort in reverse order */
	if(((Freq_t*)one)->freq < ((Freq_t*)two)->freq)
		return  1;
	else if(((Freq_t*)one)->freq == ((Freq_t*)two)->freq)
		return  0;
	else	return -1;
}

void qsort(void *base, size_t nel, size_t width, int (*compar)(const void *, const void *));
#if __STD_C
static void rlelearn(Vcrle_t* rle, Vcchar_t* data, size_t size)
#else
static void rlelearn(rle, data, size)
Vcrle_t*	rle;
Vcchar_t*	data;
size_t		size;
#endif
{
	size_t		n, c;
	Vcchar_t	*endd;
	Freq_t		*f, *endf, *freq = rle->freq;

	/* compute frequencies for bytes in training set */
	for(n = 0, f = freq; n < 256; ++n, ++f)
	{	f->freq = 0;	
		f->byte = n;
	}
	for(endd = data+size; data < endd; ++data)
		freq[*data].freq += 1;

	/* compute popular bytes */
	qsort(freq, 256, sizeof(Freq_t), freqcmp);
	for(endf = (f = freq)+(1<<RL_MAXB)-1; f < endf; ++f)
		if(f->freq == 0)
			break;

	/* popular bytes will be specially coded to take less space */
	rle->nbyte = n = f - freq;
	for(c = 1, f = freq; c <= n; ++c, ++f)
		rle->byte[c] = f->byte;
	for(c = 0; c < 256; ++c)
		freq[c].freq = 0;
	for(c = 1, data = rle->byte+1; c <= n; ++c, ++data)
	{	freq[*data].freq = 1;
		freq[*data].byte = (Vcchar_t)c;
	}
}

#if __STD_C
void vcputl(Vcchar_t** buf, Vcint_t v, Vcchar_t c, int bits)
#else
void vcputl(buf, v, c, bits)
Vcchar_t**	buf;
Vcint_t		v;	/* value to encode	*/
Vcchar_t	c;	/* start byte		*/
int		bits;	/* bits available in c	*/
#endif
{
	c |=  v & ((1 << bits) - 1);
	for(v >>= bits; v > 0; v >>= 7)
	{	c |= 128;
		vcputc(buf,c);
		c = v & 127;
	}
	vcputc(buf,c);
}

#if __STD_C
Vcint_t vcgetl(Vcchar_t** buf, Vcchar_t c, int bits)
#else
Vcint_t vcgetl(buf, c, bits)
Vcchar_t**	buf;
Vcchar_t	c;	/* start byte		*/
int		bits;	/* bits available in c	*/
#endif
{
	Vcint_t	v;

	v = c & ((1 << bits)-1);
	while(c&128)
	{	c = vcgetc(buf);
		v |= (c & 127) << bits;
		bits += 7;
	}

	return v;
}


#if __STD_C
static ssize_t vcrle(Vcodex_t* vc, const Void_t* data, size_t size,
			Void_t** out, size_t nout)
#else
static ssize_t vcrle(vc, data, size, out, nout)
Vcodex_t*	vc;
Void_t*		data;
size_t		size;
Void_t**	out;
size_t		nout;
#endif
{
	int		n, rbit, b;
	size_t		c, Bbits, Sbits, rSbits;
	Vcchar_t	*r, *d, *endd, *output, *o, *add;
	Freq_t		*freq;
	Vcrle_t		*rle = (Vcrle_t*)vc->mtdata;

	if(out && nout >= size+64)
		output = *out;
	else if(!(output = _vcbuffer(vc, size + 64, 0)) )
		return -1;

	o = output;

	if(!rle->learn)
	{	rlelearn(rle, (Vcchar_t*)data, size);

		/* output the bytes to be specially coded */
		vcputc(&o, rle->nbyte);
		for(n = 1; n <= rle->nbyte; ++n)
			vcputc(&o, rle->byte[n]);
	}

	Bbits  = RL_B(rle->nbyte);
	rbit   = RL_r(Bbits);
	rSbits = (Sbits = RL_S(Bbits)) + 1;

	freq = rle->freq; add = NIL(Vcchar_t*);
	for(endd = (d = (Vcchar_t*)data)+size; d < endd; d = r)
	{	/* see if a run starts here */	
		for(b = *d, r = d+1; r < endd; ++r)
			if(*r != b)
				break;
		if((n = r-d) > 2 || (freq[b].freq && !add) )
		{	if(add) /* put out non-run data */
			{	c = d-add;	/**/ WRADD(3,c);
				vcputl(&o, c, 0, Sbits);
				vcputs(&o, add, c);
				add = NIL(Vcchar_t*);
			}

			if(freq[b].freq)
				vcputl(&o, n, (freq[b].byte << rSbits), rSbits);
			else
			{	vcputl(&o, n, rbit, Sbits);
				vcputc(&o, b);
			} /**/ WRRUN(3,n,b);
		}
		else if(!add)
			add = d;
	}

	if(add) /* put out non-run data */
	{	c = d-add;	/**/ WRADD(3,c);
		vcputl(&o, c, 0, Sbits);
		vcputs(&o, add, c);
	}

	c = o-output;
	if(out)
	{	if(nout == 0)
			*out = (Void_t*)output;
		else if(*out != output && nout >= c)
			vcgets(&output, *out, c);
	}

	return c;
}

#if __STD_C
static ssize_t vcunrle(Vcodex_t* vc, const Void_t* data, size_t size,
			Void_t** out, size_t nout)
#else
static ssize_t vcunrle(vc, data, size, out, nout)
Vcodex_t*	vc;
Void_t*		data;
size_t		size;
Void_t**	out;
size_t		nout;
#endif
{
	int		rbit, Bbits, Sbits, rSbits, Bmask;
	size_t		b, n, c;
	Vcchar_t	*d, *endd, *output, *o, *endo, *byte, *buf;
	Vcrle_t		*rle = (Vcrle_t*)vc->mtdata;

	endd = (d = (Vcchar_t*)data) + size;

	if(out && nout > 0)
	{	o = output = *out;
		endo = o + nout;
		size = 0;
	}
	else
	{	size = ALLOCSZ(2*size);
		if(!(o = output = _vcbuffer(vc, size, 0)) )
			return -1;
		endo = o + size;
	}

	/* read the bytes that were specially coded */
	byte = rle->byte;
	if(!rle->learn)
	{	rle->nbyte = vcgetc(&d);
		for(b = 1; b <= rle->nbyte; ++b)
			byte[b] = vcgetc(&d);
	}

	Bbits  = RL_B(rle->nbyte);
	rbit   = RL_r(Bbits);
	rSbits = (Sbits = RL_S(Bbits)) + 1;
	Bmask  = (1 << Bbits) - 1;

	while(d < endd)
	{	c = vcgetc(&d);
		if((b = (c >> rSbits) & Bmask) == 0)
		{	n = vcgetl(&d, c, Sbits);
			if(c&rbit)
			{	b = vcgetc(&d);
				goto do_run;
			}

			/* ADD data, make sure there is space */
			if((o+n) > endo)
			{	if(!size)
					return -1;
				size = ALLOCSZ(size+n);
				size += ALLOCMORE(size);
				if(!(buf = _vcbuffer(vc, size, 1)) )
					return -1;
				o = buf + (o-output);
				endo = (output = buf)+size;
			}

			if(n == 1)
				*o = vcgetc(&d);
			else	vcgets(&d, o, n);
			o += n;	/**/ WRADD(4,n);
		}
		else
		{	b = byte[b];
			n = vcgetl(&d, c, rSbits);
		do_run: /**/ WRRUN(4,n,b);
			if((o+n) > endo)
			{	if(!size)
					return -1;
				size = ALLOCSZ(size+n);
				size += ALLOCMORE(size);
				if(!(buf = _vcbuffer(vc, size, 1)) )
					return -1;
				o = buf + (o-output);
				endo = (output = buf)+size;
			}
			for(; n > 0; --n)
				vcputc(&o,b);
		}
	}

	if(out && nout == 0)
		*out = output;

	return o-output;
}

#if __STD_C
static int vcrlevent(Vcodex_t* vc, int type, Void_t* params)
#else
static int vcrlevent(vc, type, params)
Vcodex_t*	vc;
int		type;
Void_t*		params;
#endif
{
	Vcrle_t*	rle;

	if(type == VC_OPENING)
	{	if(!(rle = (Vcrle_t*)malloc(sizeof(Vcrle_t))) )
			return -1;
		vc->mtdata = (Void_t*)rle;
		goto vc_disc;
	}
	else if(type == VC_DISC)
	{ vc_disc:
		if(vc->disc->size > 0)
		{	rlelearn(rle,vc->disc->data,vc->disc->size);
			rle->learn = 1;
		}
		else	rle->learn = 0;
	}
	else if(type == VC_CLOSING)
	{	if(vc->mtdata)
			free(vc->mtdata);
		vc->mtdata = NIL(Void_t*);
	}

	return 0;
}

static Vcmethod_t _Vcrle =
{	vcrle,
	vcrlevent,
	VC_MTRLE
};

static Vcmethod_t _Vcunrle =
{	vcunrle,
	vcrlevent,
	VC_MTUNRLE
};

Vcmethod_t*	Vcrle = &_Vcrle;
Vcmethod_t*	Vcunrle = &_Vcunrle;
