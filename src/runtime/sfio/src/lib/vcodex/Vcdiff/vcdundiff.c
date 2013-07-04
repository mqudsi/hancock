#include	"vcdhdr.h"

/*	Reconstruct target data from source data and delta instructions
**
**	Written by Kiem-Phong Vo (kpv@research.att.com)
*/

#ifdef DEBUG
#define GOTO_done	abort()
#else
#define GOTO_done	goto done
#endif

#if __STD_C
static ssize_t vcdundiff(Vcodex_t* vc, const Void_t* del, size_t ndel,
			Void_t** out, size_t nout)
#else
static ssize_t vcdundiff(vc, del, ndel, out, nout)
Vcodex_t*	vc;
Void_t*		del;
size_t		ndel;
Void_t**	out;
size_t		nout;
#endif
{
	Vcchar_t	*t, *s;
	int		ctrl, d, i, a, ntar, nsrc;
	Vcchar_t	*tar, *etar, *src;
	Vcchar_t	*data, *edata, *inst, *einst, *addr, *eaddr;
	Vcchar_t*	output;
	Vcdcode_t*	code;
	Vcdiff_t*	vcd;
	Vcdcache_t*	ka;
	int		event = VC_CORRUPT;

	if(!vc || !(vcd = (Vcdiff_t*)vc->mtdata))
		return -1;
	ka = vcd->cache;

	/* read size of data buffers */
	data = (Vcchar_t*)del;
	ntar = (int)vcgetu(&data);	/* buffer size for target data		*/
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
	if(!out)
		return ntar;
	else if(nout == 0)
	{	if(!(output = (Vcchar_t*)_vcbuffer(vc, ntar, 0)) )
			return -1;
	}
	else if(nout < ntar)
		return -1;
	else	output = (Vcchar_t*)(*out);

	etar = (tar = output) + ntar;

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

	/* source data */
	nsrc = vc->disc->size;
	src = vc->disc->data;

	/* clear address caches and set code table */
	vcdkaclear(ka);
	code = vcd->table->code; /**/ SET(Tab, vcd->table);

	for(t = tar; t < etar; )
	{	Vcdcode_t*	cd;
		Vcdinst_t*	in;
		int		size;

		/* get the pair of instructions */
		if(inst >= einst)
			GOTO_done;
		cd = code + *inst++;

		/**/ TALLY((cd->inst1.type == VCD_ADD && cd->inst2.type == VCD_COPY),
			   Nmerge, 1);
		/**/ TALLY((cd->inst1.type == VCD_ADD && cd->inst2.type == VCD_COPY),
			   Naddcopy[cd->inst2.mode], 1);
		/**/ TALLY((cd->inst1.type == VCD_COPY && cd->inst2.type == VCD_ADD),
			   Nmerge, 1);
		/**/ TALLY((cd->inst1.type == VCD_COPY && cd->inst2.type == VCD_ADD),
			   Ncopyadd[cd->inst1.mode], 1);

		for(i = 0; i < 2; ++i)
		{	in = i == 0 ? &cd->inst1 : &cd->inst2;
			if(in->type == VCD_NOOP)
				continue;

			if((size = in->size) == 0)
			{	if(inst >= einst)
					GOTO_done;
				size = vcgetu(&inst);
			}

			if(t+size > etar)
				GOTO_done;

			if(in->type == VCD_COPY)
			{	/**/RECORD(Ncopy,Mcopy,Tcopy,size);

				if(addr >= eaddr)
					GOTO_done;
				d = vcdkagetaddr(ka,&addr,(t-tar)+nsrc,in->mode);

				if(d < nsrc)
				{	if(d+size > nsrc)
						GOTO_done;
					s = src+d;
				}
				else
				{	d -= nsrc;
					if(d >= (t-tar) || (d+size) > ntar)
						GOTO_done;
					s = tar+d;
				}
				for(; size > 0; --size)
					*t++ = *s++;
			}
			else if(in->type == VCD_ADD)
			{	/**/ RECORD(Nadd,Madd,Tadd,size);

				if(data+size > edata)
					GOTO_done;
				for(; size > 0; --size)
					*t++ = *data++;
			}
			else if(in->type == VCD_RUN)
			{	/**/ RECORD(Nrun,Mrun,Trun,size);

				if(data >= edata)
					GOTO_done;
				d = *data++;
				for(; size > 0; --size)
					*t++ = (Vcchar_t)d;
			}
			else	GOTO_done;
		}
	}

	if(data == edata && inst == einst && addr == eaddr && t == etar)
		event = 0;	/* decode successfully */

done:
	/**/ SHOW(8,nsrc,ntar,ndel);

	if(event)
	{	if(vc->disc->eventf)
			(*vc->disc->eventf)(vc, event, NIL(Void_t*), vc->disc);
		return -1;
	}
	else
	{	if(out)
			*out = (Void_t*)tar;
		return ntar;
	}
}

#if __STD_C
static int vcduevent(Vcodex_t* vc, int type, Void_t* init)
#else
static int vcduevent(vc, type, init)
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


static Vcmethod_t _Vcundiff =
{	vcdundiff,
	vcduevent,
	VC_MTUNDIFF
};

Vcmethod_t*	Vcundiff = &_Vcundiff;
