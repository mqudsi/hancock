#include	"vcdhdr.h"

/*	Compute a delta transform from source to target.
**
**	Written by Kiem-Phong Vo (kpv@research.att.com)
*/

#define SLOP		64	/* max overhead space needed for output */

/* hash a 4-byte string */
#define H1		(101)
#define H2		(H1*H1)
#define H3		(H1*H1*H1)
#define HASH(k,f)	(k = H3*f[0] + H2*f[1] + H1*f[2] + f[3])
#define NEXT(k,f,lf)	(k = H1*(k - H3*lf[0]) + f[3])
#define KEY(k,f,lf)	((lf == f ? k : (lf+1) == f ? NEXT(k,f,lf) : HASH(k,f)), lf=f )

/* elements in the hash table chains must be kept in forward order. we do this by
   keeping a circular chain with the chain pointer points to the last element and
   the next field of this last element points to the start of the chain.
*/
#define CHAIN(c, l) \
		(c[0] = c[0] ? (l->next = c[0]->next, c[0]->next = l) : (l->next = l) )

#if __STD_C
static void vcdflsadd(Vcdiff_t* vcd, Vcchar_t* data, int size)
#else
static void vcdflsadd(vcd, data, size)
Vcdiff_t*	vcd;
Vcchar_t*	data;
int		size;
#endif
{
	int		code;
	Vcdtable_t*	tbl = vcd->table;
	Vcdsize_t*	siz = vcd->size;
	Vcdindex_t*	idx = vcd->index;

	code = idx->add[size <= siz->add ? size : 0];
	vcputc(&vcd->inext, code); /**/ COUNT(Code[code]);

	if(tbl->code[code].inst1.size == 0)
	{	vcputu(&vcd->inext, size);
		/**/ COUNT(Naddbig); TALLY(1,Saddbig,VCSIZEU(size));
	}
	else
	{	/**/ COUNT(Naddsmall);
	}

	vcputs(&vcd->dnext, data, size);
}

#if __STD_C
static void vcdflsaddr(Vcdiff_t* vcd, int best, int mode, int here)
#else
static void vcdflsaddr(vcd, best, mode, here)
Vcdiff_t*	vcd;
int		best;
int		mode;
int		here;
#endif
{
	/**/ COUNT(Naddr);
	if(mode <= VCD_HERE+vcd->cache->s_near)
	{	vcputu(&vcd->anext, best);
		/**/ TALLY(1, Saddr, VCSIZEU(best));
	}
	else
	{	vcputc(&vcd->anext, best);
		/**/ TALLY(1, Saddr, 1);
	}
}

#if __STD_C
static void vcdflscopy(Vcdiff_t* vcd, int best, int size, int mode, int here)
#else
static void vcdflscopy(vcd, best, size, mode, here)
Vcdiff_t*	vcd;
int		best;
int		size;
int		mode;
int		here;
#endif
{
	int		code;
	Vcdtable_t*	tbl = vcd->table;
	Vcdsize_t*	siz = vcd->size;
	Vcdindex_t*	idx = vcd->index;

	code = idx->copy[mode][size <= siz->copy[mode] ? size : 0];
	vcputc(&vcd->inext, code);
	/**/ COUNT(Code[code]);

	if(tbl->code[code].inst1.size == 0)
	{	vcputu(&vcd->inext, size);
		/**/ COUNT(Ncopybig); TALLY(1,Scopybig,VCSIZEU(size));
	}
	else
	{	/**/ COUNT(Ncopysmall);
	}

	vcdflsaddr(vcd, best, mode, here);
}

#if __STD_C
static void vcdputcopy(Vcdiff_t* vcd, Vcchar_t* mtch, size_t size, Vcchar_t* curf)
#else
static void vcdputcopy(vcd, mtch, size, curf)
Vcdiff_t*	vcd;
Vcchar_t*	mtch;
Size_t		size;
Vcchar_t*	curf;
#endif
{
	int		here, addr, best, mode, code;
	Vcdsave_t*	save = vcd->save;
	Vcdsize_t*	siz = vcd->size;
	Vcdindex_t*	idx = vcd->index;

	if(size > 0) /* compute values to be encoded */
	{	if(mtch >= vcd->src && mtch <= vcd->src+vcd->nsrc)
			addr = mtch - vcd->src;
		else	addr = (mtch - vcd->tar) + vcd->nsrc;
		here = (curf - vcd->tar) + vcd->nsrc;
		best = vcdkasetaddr(vcd->cache, addr, here, &mode);

		/**/ RECORD(Ncopy,Mcopy,Tcopy,size);
	}
	else	here = mode = best = addr = 0;

	if(save->size > 0 && save->data)
	{	/* there is a saved ADD instruction */
		if(size <= 0)
			vcdflsadd(vcd, save->data, save->size);
		else if(save->size <= siz->add1[mode] && size <= siz->copy2[mode])
		{	code = idx->addcopy[save->size][mode][size];
			vcputc(&vcd->inext, code);
			/**/ COUNT(Code[code]); COUNT(Naddcopy[mode]); COUNT(Nmerge);

			vcputs(&vcd->dnext, save->data, save->size);

			vcdflsaddr(vcd, best, mode, here);
			size = here = mode = best = 0;
		}
		else	vcdflsadd(vcd, save->data, save->size);
	}
	else if(save->size > 0 /* && !save->data */)
	{	/* there is a saved COPY instruction */
		vcdflscopy(vcd, save->best, save->size, save->mode, save->here);
	}

	/* save current COPY instruction */
	save->data = NIL(Vcchar_t*);
	save->size = size;
	save->best = best;
	save->here = here;
	save->mode = mode;
}

#if __STD_C
static void vcdputadd(Vcdiff_t* vcd, Vcchar_t* data, int size)
#else
static void vcdputadd(vcd, data, size)
Vcdiff_t*	vcd;
Vcchar_t*	data;
int		size;
#endif
{
	int		code;
	Vcdsave_t*	save = vcd->save;
	Vcdsize_t*	siz = vcd->size;
	Vcdindex_t*	idx = vcd->index;

	/**/ASSERT(size > 0);
	/**/RECORD(Nadd,Madd,Tadd,size);

	if(save->size > 0) /* there is a saved COPY instruction */
	{	/**/ ASSERT(!save->data);
		if(save->size <= siz->copy1[save->mode] && size <= siz->add2[save->mode])
		{	code = idx->copyadd[save->mode][save->size][size];
			vcputc(&vcd->inext, code);
			/**/ COUNT(Code[code]);COUNT(Ncopyadd[save->mode]);COUNT(Nmerge);

			vcdflsaddr(vcd, save->best, save->mode, save->here);

			vcputs(&vcd->dnext, data, size);

			data = NIL(Vcchar_t*); size = 0;
		}
		else	vcdflscopy(vcd, save->best, save->size, save->mode, save->here);
	}

	/* save current instruction */
	save->data = data;
	save->size = size;
	save->mode = save->here = save->best = 0;
}

#if __STD_C
static void vcdputrun(Vcdiff_t* vcd, int byte, int size)
#else
static void vcdputrun(vcd, byte, size)
Vcdiff_t*	vcd;
int		byte;
int		size;
#endif
{
	/**/RECORD(Nrun,Mrun,Trun,size);

	if(vcd->save->size > 0)	/* flush the saved instruction if any */
		vcdputcopy(vcd, NIL(Vcchar_t*), 0, NIL(Vcchar_t*));

	vcputc(&vcd->inext, 0);
	vcputu(&vcd->inext, size);
	vcputc(&vcd->dnext, byte);

	/**/ COUNT(Code[0]);
}

#if __STD_C
static void vcdfold(Vcdiff_t* vcd, int target)
#else
static void vcdfold(vcd, target)
Vcdiff_t*	vcd;
int		target;	/* 0: source, 1: target	*/
#endif
{
	Vcdlink_t	*tail, *l, *link, **chain;
	Vcdkey_t	key;			/* CMIN-hash key for s pos	*/
	Vcchar_t	*keyf;			/* key was computed here	*/
	size_t		len, n;
	Vcchar_t	*add, *best;		/* best match and add data	*/
	Vcchar_t	*f, *curf;		/* current pos for folding	*/
	Vcdlink_t	*ltar, *lsrc, *elsrc;	/* target&source links		*/
	Vcdlink_t	**htab;			/* hash table			*/
	Vcdkey_t	hmsk;			/* mask to index into htab	*/
	Vcchar_t	*fold, *endfold;	/* data to be folded		*/
	Vcchar_t	*src, *esrc, *tar, *etar;

	/* fast short-hands for strings and corresponding links */
	elsrc = (lsrc = vcd->lsrc) + vcd->nsrc;
	esrc = (src = vcd->src) + vcd->nsrc;
	ltar = vcd->ltar;
	etar = (tar = vcd->tar) + vcd->ntar;
	htab = vcd->htab;
	hmsk = vcd->hmsk;

	/* the string to be folded and its corresponding links */
	if(target)
		{ link = ltar; fold = tar; endfold = etar; }
	else	{ link = lsrc; fold = src; endfold = esrc; }

	keyf = add = best = NIL(Vcchar_t*); len = 0;
	for(curf = fold; curf < endfold; )
	{	/* looking for a RUN */
		if(target && !(vcd->flags&VCD_DELTAONLY) && (curf+RMIN) <= endfold)
		{	for(n = *curf, f = curf+1; f < endfold; ++f)
				if(*f != n)
					break;
			if((n = f-curf) >= RMIN)
			{	len = n;
				goto endsearch;
			}
		}

		f = curf;
	search: while(f+CMIN <= endfold) /* looking for a match */
		{	KEY(key,f,keyf);
			if(!(tail = htab[key&hmsk]) ) /* tail of (circular) chain */
				goto endsearch;
			for(l = tail->next;; l = l->next)
			{	Vcchar_t	*str, *estr, *m, *ms, *fs;

				/* see where the potential match is from */
				if(l >= lsrc && l < elsrc)
					{ str = src; estr = esrc; m = src + (l-lsrc); }
				else	{ str = tar; estr = etar; m = tar + (l-ltar); }

				/* see if there's enough data to be a match */
				if((m - str) < (f - curf) || (estr - m) < CMIN)
					goto l_next;

				/* starting points to match */
				fs = curf;
				ms = m - (n = f - fs); n >>= 1;

				if(fs[n] != ms[n] ) /* quick filter for match */
					goto l_next;
 
				/* check for longest possible match */
				if((estr - (m+CMIN)) > (n = endfold - (f+CMIN)))
					estr = m + CMIN + n;
				for(; ms < estr; ++fs, ++ms)
					if(*fs != *ms)
						break;
				if(fs < f+CMIN)
					goto l_next;
					
				/* best match so far */
				len  = fs - curf;
				best = ms - len;

				/* use last CMIN-1 byte plus one more for new search */
				f = fs - (CMIN-1);
				goto search;

			l_next:	if(l == tail)
					goto endsearch;
			}
		}

	endsearch:
		if(target)
		{	if(len > 0)
			{	if(add)
				{	vcdputadd(vcd, add, curf-add);
					add = NIL(Vcchar_t*);
				}
				if(!best)
					vcdputrun(vcd, *curf, len);
				else	vcdputcopy(vcd, best, len, curf);
			}
			else if(!add)
				add = curf;
		}

		if(len > 0)
		{	if(best)
				best = NIL(Vcchar_t*);
			else	/* RUN: add first location to table */
			{	l = link + (curf - fold);
				KEY(key,curf,keyf);
				chain = htab + (key&hmsk); CHAIN(chain, l);
			}

			curf += len; len = 0;
			f = curf - (CMIN-1);	/* add last CMIN-1 positions to table */
		}
		else
		{	f = curf;	/* add unmatched position to table */
			curf += 1;
		}

		if((!target || !(vcd->flags&VCD_DELTAONLY)) && (curf+CMIN-1) < endfold)
		{	for(l = link + (f-fold); f < curf; ++f, ++l)
			{	KEY(key,f,keyf);
				chain = htab + (key&hmsk); CHAIN(chain, l);
			}
		}
	}

done:
	if(target)
	{	if(add)
			vcdputadd(vcd, add, etar - add);

		/* flush any saved instruction */
		vcdputcopy(vcd, NIL(Vcchar_t*), 0, NIL(Vcchar_t*));
	}
}

#if __STD_C
static ssize_t vcddiff(Vcodex_t* vc, const Void_t* tar, size_t ntar,
			Void_t** del, size_t ndel)
#else
static ssize_t vcddiff(vc, tar, ntar, del, ndel)
Vcodex_t*	vc;
Void_t* 	tar;
size_t		ntar;
Void_t**	del;
size_t		ndel;
#endif
{
	Vcdkey_t	k, hmsk;
	Vcdlink_t	*l, **htab, **chain;
	Vcdsave_t	save;			/* saved instruction		*/
	Vcchar_t	ctrl, *p, *lastp, *endp;
	Vcchar_t*	output;			/* output buffer		*/
	Vcchar_t*	inst = NIL(Vcchar_t*);	/* output instructions		*/
	Vcchar_t*	addr = NIL(Vcchar_t*);	/* output COPY addresses	*/
	Vcchar_t*	data = NIL(Vcchar_t*);	/* output unprocessed data	*/
	size_t		nsrc, ninst, naddr, ndata;
	Vcdiff_t*	vcd;
	ssize_t		n, rv = -1;

	if(!vc || !(vcd = (Vcdiff_t*)vc->mtdata))
		return -1;

	if(ntar == 0)
		return 0;

	if((nsrc = vc->disc->size) < CMIN || !vc->disc->data)
		nsrc = 0;
	vcd->src  = nsrc == 0 ? NIL(Vcchar_t*) : (Vcchar_t*)vc->disc->data;
	vcd->nsrc = nsrc;
	vcd->tar  = (Vcchar_t*)tar;
	vcd->ntar = ntar;

	/* hash table size is a power-of-2 */
	n = nsrc+ntar;
	do { hmsk = n; } while ((n &= n-1) != 0);
	if((hmsk >>= 1) < 64)
		hmsk = 64;
	hmsk -= 1;

	if(hmsk != vcd->hmsk) /* allocate new table */
	{	if(vcd->htab)
			free(vcd->htab);
		if(!(vcd->htab = (Vcdlink_t**)malloc((hmsk+1)*sizeof(Vcdlink_t*))) )
			goto done;
		vcd->hmsk = hmsk;
	}
	htab = vcd->htab;
	for(n = 0; n <= hmsk; ++n)
		htab[n] = NIL(Vcdlink_t*);

	/* space for source positions */
	if(vcd->lsrc)
		free(vcd->lsrc);
	if(nsrc == 0)
		vcd->lsrc = NIL(Vcdlink_t*);
	else if(!(vcd->lsrc = (Vcdlink_t*)malloc(nsrc*sizeof(Vcdlink_t))) )
		goto done;

	/* space for target positions */
	if(vcd->ltar)
		free(vcd->ltar);
	if(ntar == 0)
		vcd->ltar = NIL(Vcdlink_t*);
	else if(!(vcd->ltar = (Vcdlink_t*)malloc(ntar*sizeof(Vcdlink_t))) )
		goto done;

	/* obtain output buffer */
	if(!(output = (Vcchar_t*)_vcbuffer(vc, 3*(ntar+SLOP), 0) ) )
		goto done;
	vcd->dbase = vcd->dnext = (Void_t*)(output+SLOP);
	vcd->ibase = vcd->inext = (Void_t*)(((Vcchar_t*)vcd->dbase) + ntar);
	vcd->abase = vcd->anext = (Void_t*)(((Vcchar_t*)vcd->ibase) + ntar+SLOP);

	/* initialize address caches and saved COPY instruction */
	vcdkaclear(vcd->cache);
	vcd->save = &save;
	save.data = NIL(Vcchar_t*);
	save.size = save.best = save.mode = save.here = 0;
	/**/ SET(Tab, vcd->table);

	if(nsrc > 0)
	{	if(!(vcd->flags&VCD_DELTAONLY) )
			vcdfold(vcd, 0);
		else
		{	/* for VCD_DELTAONLY, strings are expected to have long
			   stretches of matches. So hashing the whole source string
			   rather than parsing it is both more time-efficient and
			   producing more compact output. If this expectation turns
			   out to be false, the cost is longer matching time.
			*/
			endp = (p = vcd->src) + nsrc - CMIN;
			for(l = vcd->lsrc, HASH(k,p);; ++l)
			{	chain = htab + (k&hmsk); CHAIN(chain, l);
				lastp = p;
				if((p += 1) < endp)
					NEXT(k,p,lastp);
				else	break;
			}
		}
	}

	/* now compute the delta instructions */
	vcdfold(vcd, 1);

	/* to indicate whether the different datasets are further compressed */
	ctrl = 0;

	/* secondary processing of the unmatched data */
	ndata = (Vcchar_t*)vcd->dnext - (Vcchar_t*)vcd->dbase;
	data = (Vcchar_t*)vcd->dbase;
	if(vcd->vcdata &&
	   (n = vcapply(vcd->vcdata, vcd->dbase, ndata, &p, 0)) >= 0 && n < ndata)
	{	ndata = n;
		data = p;
		ctrl |= VCD_DATACOMP;
	}

	/* secondary processing of the instructions */
	ninst = (Vcchar_t*)vcd->inext - (Vcchar_t*)vcd->ibase;
	inst = (Vcchar_t*)vcd->ibase;
	if(vcd->vcinst &&
	   (n = vcapply(vcd->vcinst, vcd->ibase, ninst, &p, 0)) >= 0 && n < ninst)
	{	ninst = n;
		inst = p;
		ctrl |= VCD_INSTCOMP;
	}

	/* secondary processing of the COPY addresses */
	naddr = (Vcchar_t*)vcd->anext - (Vcchar_t*)vcd->abase;
	addr = (Vcchar_t*)vcd->abase;
	if(vcd->vcaddr &&
	   (n = vcapply(vcd->vcaddr, vcd->abase, naddr, &p, 0)) >= 0 && n < naddr)
	{	naddr = n;
		addr = p;
		ctrl |= VCD_ADDRCOMP;
	}

	/* encode header data */
	p = output;
	vcputu(&p, ntar);
	vcputc(&p, ctrl);
	vcputu(&p, ndata);
	vcputu(&p, ninst);
	vcputu(&p, naddr);
	n = p-output;

	/* total output size */
	rv = n + ndata + ninst + naddr;

	if(del)
	{	if(ndel == 0)
		{	*del = (Void_t*)output;
			goto cpy_data;
		}
		else if(ndel >= rv)
		{	p = *del;
			vcputs(&p, output, n);
		cpy_data:
			vcputs(&p, data, ndata);
			vcputs(&p, inst, ninst);
			vcputs(&p, addr, naddr);
		}
		else	return -1;
	}

	/**/SHOW(8, vcd->nsrc, vcd->ntar, rv);
done:
	return rv;
}

#if __STD_C
static int vcdevent(Vcodex_t* vc, int type, Void_t* init)
#else
static int vcdevent(vc, type, init)
Vcodex_t*	vc;
int		type;
Void_t*		init;
#endif
{
	Vcdiff_t*	vcd;
	int		rv = 0;

	_vcdtblinit(); /* initialize default code tables */

	if(type == VC_OPENING)
	{	if(!(vcd = (Vcdiff_t*)calloc(1,sizeof(Vcdiff_t))) )
			return -1;
		VCDINIT(vcd);
		vc->mtdata = (Void_t*)vcd;

		rv = -1;

		if(init)
		{	Vcdinit_t*	p = (Vcdinit_t*)init;

#if 0			/* currently we do not allow changing code table because there
			   is no code yet to construct an inverted index for coding
			   the delta instructions.
			*/
			vcd->table = p->codetable;
#endif
			if(p->compressor)
			{	if(!(vcd->vcdata = vcopen(0,p->compressor,0,0)))
					goto vc_close;
				if(!(vcd->vcinst = vcopen(0,p->compressor,0,0)))
					goto vc_close;
				if(!(vcd->vcaddr = vcopen(0,p->compressor,0,0)))
					goto vc_close;
			}

			if(p->deltaonly)
				vcd->flags |= VCD_DELTAONLY;
		}

		if(!vcd->table)
			vcd->table = _Vcdtbl;
		vcd->index = &_Vcdindex;
		vcd->size = &_Vcdsize;

		if(!(vcd->cache = vcdkaopen(vcd->table->s_near, vcd->table->s_same)) )
			goto vc_close;

		rv = 0;
	}
	else if(type == VC_DISC)
	{	if((vcd = vc->mtdata) )
		{	if(vcd->lsrc)
				free(vcd->lsrc);
			if(vcd->ltar)
				free(vcd->ltar);
			if(vcd->htab)
				free(vcd->htab);
			vcd->lsrc = vcd->ltar = NIL(Vcdlink_t*);
			vcd->htab = NIL(Vcdlink_t**);
			vcd->hmsk = 0;
		}
	}
	else if(type == VC_CLOSING)
	{ vc_close:
		if((vcd =  vc->mtdata) )
		{	if(vcd->vcdata)
				vcclose(vcd->vcdata);
			if(vcd->vcinst)
				vcclose(vcd->vcinst);
			if(vcd->vcaddr)
				vcclose(vcd->vcaddr);
			if(vcd->cache)
				vcdkaclose(vcd->cache);

			if(vcd->lsrc)
				free(vcd->lsrc);
			if(vcd->ltar)
				free(vcd->ltar);
			if(vcd->htab)
				free(vcd->htab);

			free(vcd);
		}
		vc->mtdata = NIL(Void_t*);
	}

	return rv;
}

static Vcmethod_t _Vcdiff =
{	vcddiff,
	vcdevent,
	VC_MTDIFF
};

Vcmethod_t*	Vcdiff = &_Vcdiff;
