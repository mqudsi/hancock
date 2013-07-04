/* CRC=0116736410 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fixcut.h"

/* cut-style functionality for fixed-length records
**
** XXX For now, assume records are multiples of 8 bits.
** Allow fields aligned on arbitrary bit boundaries.
*/


static unsigned int Lobits[] = {
    0x0000, 0x0001, 0x0003, 0x0007, 0x000f, 0x001f, 0x003f, 0x007f,
    0x00ff, 0x01ff, 0x03ff, 0x07ff, 0x0fff, 0x1fff, 0x3fff, 0x7fff
};


/* To simplify freeing all the space we allocate,
** maintain a list of allocations on FreeChain.
**
** Since fchandles are, themselves, dynamically allocated,
** we want to include the fchandle on its own FreeChain.
** We do this with a temporary fchandle, whose FreeChain,
** containing just the space for the dynamic fchandle,
** is copied into the newly allocated fchandle.
*/

void *
fcalloc(fchandle *fcp, size_t size)
{
    void *vp;
    freelink *freep;

    size += sizeof(freelink);
    if ((vp = calloc(1,size)) == NULL) {
	fprintf(stderr,"Out of memory in fcalloc going after %u bytes\n", size);
	exit(2);
    }
    freep = (freelink *) vp;
    vp = ((freelink *)vp) + 1;
    freep->next = fcp->FreeChain;
    fcp->FreeChain = freep;
    return vp;
}

fchandle *
fcopen(void)
{
    fchandle fctemp, *fcp;	/* It takes a handle to allocate a handle */

    fctemp.FreeChain = NULL;			/* Nothing to free (yet) */
    fcp = fcalloc(&fctemp, sizeof(fchandle));	/* Allocate new handle */
    fcp->FreeChain = fctemp.FreeChain;		/* Free self last */
    return fcp;
}

void
fcclose(fchandle *fcp)
{
    freelink *thisp, *nextp;

    for (thisp = fcp->FreeChain; thisp; thisp = nextp) {
	nextp = thisp->next;
	free(thisp->mustfree);
    }
}


void
bitcat(seg *tsp, seg *fsp)
{
    boundary tb, fb, n;
    long size;				/* unsigned is too clumsy here */
    uchar *tp, *fp;
    int tx, fx, left;

    tb = tsp->bounds.start;		/* (end of active) target boundary */
    fb = fsp->bounds.start;		/* (start of) source boundary */

    size = tsp->bounds.stop - tb;	/* bits still available at target */
    n    = fsp->bounds.stop - fb;	/* bits at source */
    if (size > n) size = n;		/* bits to be copied */
    if (size == 0) return;		/* nothing to do */
    tsp->bounds.start += size;		/* regard the target space as used */

    tp = tsp->base + BYTES(tb);		/* first byte of target */
    fp = fsp->base + BYTES(fb);		/* first byte of source */
    tx = BITS(tb);			/* bits in use at target byte */
    fx = BITS(fb);			/* bits in use at source byte */

    /* If the bit offsets of target and source are the same,
    ** we don't have to shift chars as we copy them.
    ** Just take care of any odd bits at the start, then copy entire bytes.
    ** It's ok to copy ``extra'' bits.  There will always be space for them,
    ** and they'll simply be ignored, or overwritten later.
    */

    if (tx == fx) {		/* good deal, we're aligned the same */
	if (tx) {		/* we have used some bits in the 1st byte */
	    left = BITSPERCHAR - tx;		/* how many are still unused */
	    *tp   &=        ~Lobits[left];	/* clear the unused bits */
	    *tp++ |= *fp++ & Lobits[left];	/* or in from the source */
	    size -= left;			/* account for these bits */
	}
	while (size > 0) {	/* Byte or partial byte remains, aligned */
	    *tp++ = *fp++;
	    size -= BITSPERCHAR;
	}
    } else {			/* source and target misaligned */
	/* We are going to use variable n as a ``one-byte buffer''
	** of source bits.  We will maintain the invariant that the
	** byte contains fx bits from the byte currently addressed by fp,
	** and (BITSPERCHAR - fx) bits from the previous byte.
	*/
	n = *fp++;		/* load complete source byte */
	if (fx) {		/* some bits are in the next byte */
	    n <<= BITSPERCHAR;
	    n |= *fp;
	    n >>= fx;
	}
	/* Next source byte now complete in n, invariant applies.
	**
	** Begin by filling out the target byte to a byte boundary,
	** so we can complete the copy with the target aligned.
	*/
	if (tx) {			/* partial 1st byte */
	    left = BITSPERCHAR - tx;	/* bits still unused */
	    *tp &= ~Lobits[left];	/* clear the unused bits */
	    /* Align and ``or in'' from the source byte */
	    *tp++ |= (n >> tx) & Lobits[left];
	    size -= left;		/* account for these bits */
	    /* We just gobbled ``left'' bits from the source byte in n.
	    ** Shift the entire byte over, leaving the unused tx bits
	    ** adjacent to the high order bits of the low byte.
	    ** Load the low byte with the remaining bits from the byte at fp.
	    ** If the new source boundary is into the next byte,
	    ** load as many bits as necessary from there.
	    ** Leave the invariant about fp and n and fx valid.
	    */
	    n <<= BITSPERCHAR;
	    n |= (((*fp) << fx) & BYTEMASK);
	    fx += left;
	    if (fx >= BITSPERCHAR) {
		fp++;
		fx -= BITSPERCHAR;
		if (fx > 0) {
		    n |= ((*fp) >> (BITSPERCHAR - fx)) & Lobits[fx];
		}
	    }
	    /* Having now loaded (at least) ``left'' bits into
	    ** the byte buffer in n, shift any excess out,
	    ** replacing them with the corresponding unused bits.
	    */
	    n >>= tx;
	    n &= BYTEMASK;
	}
	/* At this point, the target bits are byte-aligned.
	** The source bits are *not* (or we would have taken
	** the if-branch of the fx == tx check),
	** the next BITSPERCHAR bits (if any) are in n.
	** Stuff the bits into the target, and load the next batch.
	*/
	while (size > 0) {	/* byte or partial byte remain */
	    *tp++ = n;
	    n = (*fp++) << BITSPERCHAR;
	    n |= *fp;
	    n >>= fx;
	    size -= BITSPERCHAR;
	}
    }
    return;
}


static uchar Zbyte[1];

/* Convert from <byte>[.<bit>] notation to boundary */

boundary
strtoboundary(const char *str, char **ptr)
{
    boundary b;
    long bytes;
    long bits;
    char *p;

    bits = 0;
    errno = 0;
    bytes = strtol(str, &p, 0);
    if ((errno == 0) && (*p == '.')) bits = strtol(p+1, &p, 0);
    b = BUILD(bytes, bits);
    if (ptr != NULL) *ptr = p;
    return(b);
}

char *
boundarytostr(boundary b)
{
    static char buf[10+6];
    long bytes, bits;

    bytes = BYTES(b);
    bits  =  BITS(b);
    if (bits == 0) {
	sprintf(buf, "%ld", bytes);
    } else {
	sprintf(buf, "%ld.%ld", bytes, bits);
    }
    return (buf);
}


static seg *
newseg(fchandle *fcp)
{
    seg *segp = fcp->SegChain;
    seg *sp;
    int n;

    if (segp == NULL) {
	n = 25;
	segp = sp = (seg *) fcalloc(fcp, n * sizeof(seg));
	while (--n) {
	    sp->next = sp + 1;
	    sp++;
	}
    }
    sp = segp;
    fcp->SegChain = sp->next;
    sp->next = NULL;
    return(sp);
}


static oplink *
newoplink(fchandle *fcp)
{
    oplink *oplist = fcp->OpChain;
    oplink *op;
    int n;

    if (oplist == NULL) {
	n = 100;
	oplist = op = (oplink *) fcalloc(fcp, n * sizeof(oplink));
	while (--n) {
	    op->next = op + 1;
	    op++;
	}
    }
    op = oplist;
    fcp->OpChain = op->next;
    op->next = NULL;
    return(op);
}


static int
parseops(fchandle *fcp, seg *segp, char *p, char **savep)
{
    oplink *op, **lastop;
    int wascond = 0;
    int skipnext = 0;
    int minsize;
    long size;
    static unsigned short testEndian = 0xff00;
    uchar bigendian = *(uchar *)(&testEndian);


    size = segp->bounds.stop;
    if (size) size -= segp->bounds.start;
    lastop = &(segp->chain);
    for (wascond = skipnext = 0;; p++) {
	/* First, some common logic for conditionals,
	** real operations, and completion
	*/
	minsize = 1;	/* smallest size for byte-aligned operators */
	switch (*p) {
	case 'B':
	case 'L':
	case 'T':
	case 'F':
	    if (wascond) {
	      strcpy(fcp->Error, "Conditional follows conditional");
		return(-1);
	    }
	    wascond = *p;
	    break;
	/* q, s and b have non-trivial size restrictions.
	** For example, b (byte swap) make no sense unless
	** an even number of bytes are involved.
	** The byte-swap operators AND the r byte-reversal
	** operator must involve whole bytes, no leftover bits.
	*/
	case 'q':
	    minsize *= 2;
	    /* fall into ... */
	case 's':
	    minsize *= 2;
	    /* fall into ... */
	case 'b':
	    minsize *= 2;
	    /* fall into ... */
	case 'r':
	    if (BITS(size)) {
		sprintf(fcp->Error,
		    "Size (%s) must be an even byte multiple for %c operations",
			boundarytostr(size), *p);
		return -1;
	    }
	    if (BYTES(size) % minsize) {
		sprintf(fcp->Error,
		    "Size %s is not a multiple of %c swap-unit size %d",
			boundarytostr(size), *p, minsize);
		return -1;
	    }
	    /* fall into ... */
	case 'c': case 'h': case 'l':		/* arbitrary alignment ok */
	    wascond = 0;
	    if (skipnext) {
		skipnext = 0;
		continue;
	    }
	    break;
	default:
	    if (wascond) {
		sprintf(fcp->Error,
		    "Final operator was conditional (%c)", wascond);
		return -1;
	    }
	    if (savep) *savep = p;
	    return 0;
	}
	/* Handle endian conditionals inline, set up operator for others */
	switch (*p) {
	case 'B':
	    skipnext = !bigendian;
	    break;
	case 'L':
	    skipnext = bigendian;
	    break;
	default:
	    op = newoplink(fcp);
	    op->operator = *p;
	    *lastop = op;
	    lastop = &(op->next);
	    break;
	}
    }
    /* notreached */
    return -1;
}


int
fcalign(fchandle *fcp, long align)
{
    seg *segp;

    if (errno || (align < 0) || (align >= BITSPERCHAR)) {
	sprintf(fcp->Error,
	    "Bit-alignment argument %ld out of range: 0 <= arg < %ld\n",
	    align, BITSPERCHAR);
	return -1;
    }

    segp = newseg(fcp);
    segp->key		= 'a';
    segp->base		= Zbyte;
    segp->bounds.start	= 0;
    segp->bounds.stop	= align;
    segp->next		= fcp->SegList;
    fcp->SegList = segp;
    return 0;
}


int
fcslice(fchandle *fcp, char keyletter, char *description)
{
    seg tempseg, *segp;
    char *p = description;
    long align;

    switch (keyletter) {
    case 'a':				/* External form of align */
	*(fcp->Error) = '\0';		/* Clear error */
	align = strtol(p, &p, 0);
	if (errno || *p || fcalign(fcp, align)) {
	    if (*(fcp->Error) != '\0') { /* Failure other than fcalign() */
		strcpy(fcp->Error, "Alignment argument cannot be parsed");
		return -1;
	    }
	}
	return 0;
    case 'e': case 'E': case 'k':	/* Legal keyletters */
	break;
    default:
	sprintf(fcp->Error,
	    "Unrecognized slice keyletter %c (%#o)", keyletter, keyletter);
	return -1;
    }
    tempseg.bounds.start = strtoboundary(p, &p);
    if (errno) goto bounderr;
    tempseg.bounds.stop  = 0;
    tempseg.chain = NULL;
    tempseg.key = 'k';			/* All the same (different from a) */
    if (*p == ',') {
	tempseg.bounds.stop = strtoboundary(p+1, &p);
	if (errno) goto bounderr;
	if (tempseg.bounds.stop <= tempseg.bounds.start) {
	    sprintf(fcp->Error,
		"Stop boundary %s not later than start ",
		boundarytostr(tempseg.bounds.stop));
	    strcat(fcp->Error, boundarytostr(tempseg.bounds.start));
	    return -1;
	}
    }
    if (*p && parseops(fcp, &tempseg, p, &p)) return -1;
    if (*p) {
bounderr:
	sprintf(fcp->Error,
	    "Error in %c slice boundary specifications", keyletter);
	return -1;
    }
    if ((keyletter == 'e') || (keyletter == 'E')) {
	if (tempseg.bounds.stop == 0) {
	    tempseg.bounds.stop = tempseg.bounds.start +
		    (BITSPERCHAR *
			((keyletter == 'e') ? sizeof(float) : sizeof(double)));
	    if (tempseg.chain == NULL) parseops(fcp, &tempseg, "LrTlh", NULL);
	}
    }
    if (tempseg.bounds.stop == 0) {
	if (fcp->RecBound < tempseg.bounds.start) {
	    fcp->RecBound = tempseg.bounds.start;
	}
    } else if (fcp->RecBound < tempseg.bounds.stop) {
	fcp->RecBound = tempseg.bounds.stop;
    }
    segp = newseg(fcp);
    *segp = tempseg;
    segp->next = fcp->SegList;
    fcp->SegList = segp;
    return 0;
}


char *
fcerror(fchandle *fcp)
{
    return fcp->Error;
}


/* Segments are added to the front of the SegList chain,
** but slices must be added in the order of occurrence.
** Reverse the chain, so slices appear in the proper order.
*/

static void
reverseg(fchandle *fcp)
{
    seg *revp, *fwdp, *temp;

    /* chain at revp is in reverse order
    ** chain at fwdp is properly reversed
    */
    for (revp = fcp->SegList, fwdp = NULL; revp; revp = temp) {
	temp = revp->next;
	revp->next = fwdp;
	fwdp = revp;
    }
    fcp->SegList = fwdp;
}


static int
checkops(fchandle *fcp, seg *segp)
{
    int minsize = 0;
    long size = segp->bounds.stop - segp->bounds.start;
    int operator;
    oplink *op;
    
    for (op = segp->chain; op; op = op->next) {
	switch (op->operator) {
	case 'r':
	    if (minsize < 1) operator = 'r', minsize = 1;
	    break;
	case 'b':
	    if (minsize < 2) operator = 'b', minsize = 2;
	    break;
	case 's':
	    if (minsize < 4) operator = 's', minsize = 4;
	    break;
	case 'q':
	    if (minsize < 8) operator = 'q', minsize = 8;
	    break;
	}
    }
    if (minsize) {
	if (BITS(size)) {
	    sprintf(fcp->Error,
	    	"Size (%s) of %c operator must be an even byte multiple",
		    boundarytostr(size), operator);
	    return -1;
	}
	if (BYTES(size) % minsize) {
	    sprintf(fcp->Error,
	    	"Size (%s) of %c operator must be a multiple of %d",
		    boundarytostr(size), operator, minsize);
	    return -1;
	}
    }
    return 0;
}


	
long
fcready(fchandle *fcp, long reclen)
{
    seg *segp, **headp;
    long j;
    boundary k;
    boundary l = BUILD(reclen, 0);

    if (fcp->KeyBase) {
	sprintf(fcp->Error,
	    "Length already frozen at %ld", BYTES(fcp->KeyBound));
	return -1;
    }
    if (fcp->RecBound > l) {
	sprintf(fcp->Error,
	    "Boundary %s greater than record length %ld",
		boundarytostr(fcp->RecBound), reclen);
	return -1;
    }
    fcalign(fcp, 0);	/* Force key to end up byte-aligned */
    reverseg(fcp);
    headp = &fcp->SegList;	/* where useful operators get added */
    k = 0;			/* size of accumulated slices */
    for (segp = fcp->SegList; segp; segp = segp->next) {
	if (segp->key == 'a') {			/* an alignment */
	    j = segp->bounds.stop;		/* bit to align on */
	    j -= BITS(k);			/* relative to where we are */
	    if (j < 0) j += BITSPERCHAR;	/* bits to adjust */
	    if (j == 0) continue;		/* already correct */
	    segp->bounds.stop = j;
	    k += j;
	} else {				/* copy from record */
	    if (segp->bounds.stop == 0) {
		if (segp->bounds.start == l) continue;	/* empty */
		segp->bounds.stop = l;
		if (checkops(fcp, segp)) return -1;
	    }
	    k += segp->bounds.stop - segp->bounds.start;
	}
	*headp = segp;
	headp = &(segp->next);
    }
    *headp = NULL;
    fcp->KeyBound = k;
    fcp->RecBound = l;
    j = BYTES(k);
    fcp->KeyBase = (uchar *)fcalloc(fcp, 2 * j);
    fcp->TmpBase = fcp->KeyBase + j;
    return j;
}


/* The next few routines operate on bytes that have been copied
** to the start of a temp area.  The start offset is therefore 0,
** and there's no need to worry about alignment.
*/

static void
swap(seg *segp, unsigned int step)
{
    long size = BYTES(segp->bounds.stop);
    long stringoff;
    unsigned int stepoff;
    uchar *to, *from, tmp;

    for (stringoff = 0; stringoff < size; stringoff += step * 2) {
	to = segp->base + stringoff;
	from = to + step;
	for (stepoff = 0; stepoff < step; stepoff++) {
	    tmp = *to;
	    *to++ = *from;
	    *from++ = tmp;
	}
    }
}


static void
reverse(seg *segp)
{
    uchar *to, *from, tmp;

    to = segp->base;
    from = to + BYTES(segp->bounds.stop);
    while (--from > to) {
	tmp = *to;
	*to++ = *from;
	*from = tmp;
    }
}


static void
flip(seg *segp)
{
    uchar *to;
    long size;

    to = segp->base;
    for (size = BYTES(segp->bounds.stop); size > 0; --size) {
	*to++ ^= BYTEMASK;
    }
    /* If there is a partial byte, flip the entire byte.
    ** The temp space is byte aligned, so we run no risk of accessing
    ** unallocated bytes.  We don't care about bits beyond the logical end,
    ** so flipping them or not flipping them makes no flippin' difference.
    */
    if (BITS(segp->bounds.stop)) *to ^= BYTEMASK;
}


static void
copyseg(seg *to, seg *from)
{
    to->bounds.start = 0;			/* Empty the temp buffer */
    to->bounds.stop = from->bounds.stop;	/* A lie, but a safe one */
    bitcat(to, from);				/* Copy contents in */
    to->bounds.stop = to->bounds.start;		/* Reflect current contents */
    to->bounds.start = 0;
}


/* Build the key for the current record.
**
** All the hard work was already done.
** The list of segs at SegList are sub-bitstrings of the current record,
** or, for aligns, sub-bitstrings of a byte of 0 bits.
** Any funny operations are chained together off the chain pointer.
** If there are NO funny operators, copy directly.
** Otherwise, copy into a temp area, fiddle the data there,
** and copy into place when all fiddling is complete.
*/

uchar *
fcbuild(fchandle *fcp, uchar *record, uchar *key)
{
    seg keyseg, tmpseg;
    seg *segp;
    oplink *op;
    int o, t;

    if (fcp->KeyBase == NULL) {
	strcpy(fcp->Error, "fcready() must be called before fcbuild()");
	return NULL;
    }
    /* Use user-supplied area, if any, otherwise, use pre-allocated space */
    keyseg.base = (key ? key : fcp->KeyBase);
    keyseg.bounds.start = 0;			/* clear the key buffer */
    keyseg.bounds.stop  = fcp->KeyBound;
    tmpseg.base = fcp->TmpBase;
    for (segp = fcp->SegList; segp; segp = segp->next) {
	if (segp->key != 'a') segp->base = record;
	if ((op = segp->chain) == NULL) {
	    /* simple copy */
	    bitcat(&keyseg, segp);
	} else {
	    copyseg(&tmpseg, segp);		/* copy & byte align in temp */
	    do {
		switch (o = op->operator) {
		case 'T':
		case 'F':
		    t = (*tmpseg.base & HIGHBIT); /* mask off all but hi bit */
		    /* skip if bit doesn't match expectation */
		    if ((t && (o == 'F')) || (!t && (o == 'T'))) {
			/* We disallowed conditionals with nothing following,
			** so op->next should never be NULL.
			** But the cost of the check is microscopic,
			** and it avoids running off the list.
			*/
			if (op->next) op = op->next;
		    }
		    break;
		case 'h':			/* flip hi bit only */
		    *tmpseg.base ^= HIGHBIT;
		    break;
		case 'c':			/* flip ALL bits */
		    flip(&tmpseg);
		    break;
		case 'l':			/* flip all BUT hi bit */
		    *tmpseg.base ^= HIGHBIT;	/* preflip hi */
		    flip(&tmpseg);		/* flip all, unflipping hi */
		    break;
		case 'r':
		    reverse(&tmpseg);
		    break;
		case 'b':
		    swap(&tmpseg, 1);
		    break;
		case 's':
		    swap(&tmpseg, 2);
		    break;
		case 'q':
		    swap(&tmpseg, 4);
		    break;
		default:
		    fprintf(stderr,
			"fcbuild logic error, operator %c (%d) unexpected\n",
			    o, o);
		    exit(1);
		}
		op = op->next;
	    } while (op != NULL);
	    bitcat(&keyseg, &tmpseg);		/* copy fiddled data */
	}
    }
    return keyseg.base;
}
