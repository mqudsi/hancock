#include	"vcdhdr.h"

/*	Transform Vcdiff format to Gdiff format.
**
**	Written by Kiem-Phong Vo (kpv@research.att.com)
*/

#ifdef DEBUG
#define GOTO_done	abort()
#else
#define GOTO_done	goto done
#endif

#if __STD_C
static ssize_t vcd2gdiff(Vcodex_t* vc, const Void_t* del, size_t ndel,
			Void_t** out, size_t nout)
#else
static ssize_t vcd2gdiff(vc, del, ndel, out, nout)
Vcodex_t*	vc;
Void_t*		del;
size_t		ndel;
Void_t**	out;
size_t		nout;
#endif
{
	int		ctrl, d, i, a, n, nsrc, here;
	Vcchar_t	*g, *gdiff, *egdiff;
	Vcchar_t	*data, *edata, *inst, *einst, *addr, *eaddr;
	Vcchar_t*	output;
	Vcdcode_t*	code;
	Vcdiff_t*	vcd;
	Vcdcache_t*	ka;
	Vcint_t		offset;
	int		event = VC_CORRUPT;

	if(!vc || !(vcd = (Vcdiff_t*)vc->mtdata))
		return -1;
	ka = vcd->cache;

	/* read size of data buffers */
	data = (Vcchar_t*)del;
	n = (int)vcgetu(&data);		/* buffer size for target data		*/
	ctrl = (int)vcgetc(&data);	/* to see if datasets were compressed	*/
	d = (int)vcgetu(&data); 	/* size of unmatched data		*/
	i = (int)vcgetu(&data); 	/* size of instruction set		*/
	a = (int)vcgetu(&data); 	/* size of COPY addresses		*/

	/* make sure we have enough data for decoding */
	if((d+i+a) != (ndel - (data - (Vcchar_t*)del)) )
		GOTO_done;

	/* space for instructions and COPY addresses */
	inst = data + d;
	addr = inst + i;

	/* buffer for reconstructed data */
	n = 2*n + 64;
	if(!(output = (Vcchar_t*)_vcbuffer(vc, n, 0)) )
		return -1;
	else	egdiff = (g = gdiff = output) + n;

	/* recompute the data, instruction and address streams if encoded */
	if(ctrl&VCD_DATACOMP)
	{	if(!vcd->vcdata || (d = vcapply(vcd->vcdata, data, d, &edata, 0)) < 0)
			GOTO_done;
		data = edata;
	}
	edata = data+d;

	if(ctrl&VCD_INSTCOMP)
	{	if(!vcd->vcinst || (i = vcapply(vcd->vcinst, inst, i, &einst, 0)) < 0)
			GOTO_done;
		inst = einst;
	}
	einst = inst+i;

	if(ctrl&VCD_ADDRCOMP)
	{	if(!vcd->vcaddr || (a = vcapply(vcd->vcaddr, addr, a, &eaddr, 0)) < 0)
			GOTO_done;
		addr = eaddr;
	}
	eaddr = addr+a;

	/* file offset */
	offset = vc->disc->offset;

	/* clear address caches and set code table */
	vcdkaclear(ka);
	code = vcd->table->code; /**/ SET(Tab, vcd->table);

	for(here = nsrc = vc->disc->size; ; )
	{	Vcdcode_t*	cd;
		Vcdinst_t*	in;
		unsigned int	size;

		if(g >= egdiff)
			GOTO_done;

		/* get the pair of instructions */
		if(inst >= einst)
			break;
		cd = code + vcgetc(&inst);

		for(i = 0; i < 2; ++i)
		{	in = i == 0 ? &cd->inst1 : &cd->inst2;
			if(in->type == VCD_NOOP)
				continue;

			if((size = in->size) == 0)
			{	if(inst >= einst)
					GOTO_done;
				size = vcgetu(&inst);
			}

#define MAXUCHAR	(unsigned int)((1 << 8) - 1)
#define MAXUSHORT	(unsigned int)((1 << 16) - 1)
#define MAXUINT		(unsigned int)(~((unsigned int)0))

			if(in->type == VCD_ADD)
			{	if(data+size > edata)
					GOTO_done;

				if((g + 8) > egdiff)
					GOTO_done;

				if(size <= 246)
					vcputc(&g,size);
				else if(size <= MAXUSHORT)
				{	vcputc(&g,247);
					vcputm(&g,size,MAXUSHORT);
				}
				else
				{	vcputc(&g,248);
					vcputm(&g,size,MAXUINT);
				}

				if((g + size) > egdiff)
					GOTO_done;
				vcgets(&data, g, size);
				g += size;
				here += size;
			}
			else if(in->type == VCD_COPY)
			{	if(addr >= eaddr)
					GOTO_done;

				d = vcdkagetaddr(ka,&addr,here,in->mode);
				here += size;

				if(d >= nsrc || (d+size) > nsrc || (g+16) > egdiff)
					GOTO_done;

				d += offset;
				if(d <= MAXUSHORT )
				{	if(size <= MAXUCHAR)
					{	vcputc(&g,249);
						vcputm(&g,d,MAXUSHORT);
						vcputm(&g,size,MAXUCHAR);
					}
					else if(size <= MAXUSHORT)
					{	vcputc(&g,250);
						vcputm(&g,d,MAXUSHORT);
						vcputm(&g,size,MAXUSHORT);
					}
					else
					{	vcputc(&g,251);
						vcputm(&g,d,MAXUSHORT);
						vcputm(&g,size,MAXUINT);
					}
				}
				else
				{	if(size <= MAXUCHAR)
					{	vcputc(&g,252);
						vcputm(&g,d,MAXUINT);
						vcputm(&g,size,MAXUCHAR);
					}
					else if(size <= MAXUSHORT)
					{	vcputc(&g,253);
						vcputm(&g,d,MAXUINT);
						vcputm(&g,size,MAXUSHORT);
					}
					else
					{	vcputc(&g,254);
						vcputm(&g,d,MAXUINT);
						vcputm(&g,size,MAXUINT);
					}
				}
			}
			else	GOTO_done;
		}
	}

	if(data == edata && inst == einst && addr == eaddr )
		event = 0;	/* decode successfully */

done:
	if(event)
	{	if(vc->disc->eventf)
			(*vc->disc->eventf)(vc, event, NIL(Void_t*), vc->disc);
		return -1;
	}
	else
	{	ssize_t  ngdiff = g-gdiff;
		if(out)
		{	if(nout == 0)
				*out = gdiff;
			else if(nout >= ngdiff)
				vcgets(&gdiff, *out, ngdiff);
			else	return -1;
		}
		return ngdiff;
	}
}

#if __STD_C
static int vcdgevent(Vcodex_t* vc, int type, Void_t* init)
#else
static int vcdgevent(vc, type, init)
Vcodex_t*	vc;
int		type;
Void_t*		init;
#endif
{
	Vcdiff_t*	vcd;
	int		rv = 0;

	_vcdtblinit(); /* construct default code tables */

	if(type == VC_OPENING)
	{	if(!(vcd = (Vcdiff_t*)malloc(sizeof(Vcdiff_t))) )
			return -1;
		VCDINIT(vcd);
		vc->mtdata = (Void_t*)vcd;

		rv = -1;

		if(init)
		{	Vcdinit_t*	p = (Vcdinit_t*)init;

			vcd->table = p->codetable;
			if(p->compressor)
			{	if(!(vcd->vcdata = vcopen(0,p->compressor,0,0)) )
					goto vc_close;
				if(!(vcd->vcinst = vcopen(0,p->compressor,0,0)) )
					goto vc_close;
				if(!(vcd->vcaddr = vcopen(0,p->compressor,0,0)) )
					goto vc_close;
			}
		}
		if(!vcd->table)
			vcd->table = _Vcdtbl;
		if(!(vcd->cache = vcdkaopen(vcd->table->s_near, vcd->table->s_same)) )
			goto vc_close;

		rv = 0;
	}
	else if(type == VC_CLOSING)
	{ vc_close:
		if((vcd = vc->mtdata) )
		{	if(vcd->vcdata)
				vcclose(vcd->vcdata);
			if(vcd->vcinst)
				vcclose(vcd->vcinst);
			if(vcd->vcaddr)
				vcclose(vcd->vcaddr);
			if(vcd->cache)
				vcdkaclose(vcd->cache);

			free(vcd);
		}
		vc->mtdata = NIL(Void_t*);
	}

	return rv;
}


static Vcmethod_t _Vc2gdiff =
{	vcd2gdiff,
	vcdgevent,
	VC_MT2GDIFF
};

Vcmethod_t*	Vc2gdiff = &_Vc2gdiff;
