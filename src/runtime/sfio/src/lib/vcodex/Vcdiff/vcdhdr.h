#ifndef _VCDHDR_H
#define _VCDHDR_H	1

#include	"vchdr.h"
#include	<vcdiff.h>

#ifdef DEBUG
static Vcdtable_t*	Tab;
#define VCDNEAR(m)	((m) >= VCD_HERE+1 && (m) < VCD_HERE+1+Tab->s_near)
#define VCDMODE()	(Tab->s_same + Tab->s_near + 2)
static int		Nadd, Madd, Tadd, Nrun, Mrun, Trun, Ncopy, Mcopy, Tcopy;
static int		Naddr, Saddr; /* number of copy addresses and code size */
static int		Nmerge, Ncopyadd[VCD_ADDR], Naddcopy[VCD_ADDR];
static int		Naddsmall, Naddbig, Saddbig;
static int		Ncopysmall, Ncopybig, Scopybig;
static int		Code[256];
#define RECORD(n,m,s,v)	(((n) += 1), ((m) = (m) < (v) ? (v) : (m)), ((s) += (v)) )
#define NAME(inst)	((inst)->type == VCD_RUN ? 'R' : \
			 (inst)->type == VCD_ADD ? 'A' : \
			 (inst)->mode == VCD_SELF ? 'S' : \
			 (inst)->mode == VCD_HERE ? 'H' : \
			 VCDNEAR((inst)->mode) ? 'n' : 's' )
#define PRINST(fd,inst,size) \
			{ char buf[128]; \
			  sprintf(buf,"\t%c %4d",NAME(inst),size); \
			  write(fd, buf, strlen(buf)); \
			}
#define PRLINE(fd) 	write(fd, "\n", 1)
#define SHOW(fd,ns,nt,nd) \
			{ char buf[128]; int i; \
			  sprintf(buf,"nsrc=%d ntar=%d ndel=%d\n", \
			  	  ns, nt, nd); write(fd, buf, strlen(buf)); \
			  sprintf(buf,"Nadd=%d Madd=%d Tadd=%d\n", \
			  	  Nadd, Madd, Tadd); write(fd, buf, strlen(buf)); \
			  sprintf(buf,"Nrun=%d Mrun=%d Trun=%d\n", \
			  	  Nrun, Mrun, Trun); write(fd, buf, strlen(buf)); \
			  sprintf(buf,"Ncopy=%d Mcopy=%d Tcopy=%d\n", \
			  	  Ncopy, Mcopy, Tcopy); write(fd, buf, strlen(buf)); \
			  sprintf(buf,"Naddr=%d Saddr=%d\n", \
			  	  Naddr, Saddr); write(fd, buf, strlen(buf)); \
			  sprintf(buf,"Nmerge=%d\n", \
			  	  Nmerge); write(fd, buf, strlen(buf)); \
			  sprintf(buf,"Naddsmall=%d Naddbig=%d Saddbig=%d\n", \
			  	  Naddsmall, Naddbig, Saddbig); \
				 write(fd, buf, strlen(buf)); \
			  sprintf(buf,"Ncopysmall=%d Ncopybig=%d Scopybig=%d\n", \
			  	  Ncopysmall, Ncopybig, Scopybig); \
				 write(fd, buf, strlen(buf)); \
			  PRLINE(fd); \
			  for(i = 0; i < VCDMODE(); ++i) \
			  {  sprintf(buf,"Ncopyadd[%d]=%d Naddcopy[%d]=%d\n", \
			  	  i, Ncopyadd[i], i, Naddcopy[i]); \
			     write(9, buf, strlen(buf)); \
			  } \
			  for(i = 0; i < 256; ++i) \
			  {  sprintf(buf, "%3d=%4d: ",i,Code[i]); \
			     write(9,buf,strlen(buf)); \
			     PRINST(9, &Tab->code[i].inst1, Tab->code[i].inst1.size); \
			     if(Tab->code[i].inst2.type != VCD_NOOP) \
			        PRINST(9,&Tab->code[i].inst2,Tab->code[i].inst2.size); \
			     PRLINE(9); \
			  } \
			}
#else
#define RECORD(n,m,s,v)
#define PRINST(fd,inst,size)
#define PRLINE(fd)
#define SHOW(fd,ns,nt,nd)
#endif

#define CMIN		(4)	/* min size for a COPY 		*/
#define RMIN		(16)	/* min size for a RUN		*/

#define Vcuint16_t	unsigned _ast_int16_t

/* assumed max sizes for various instruction types. The current Vcdiff
   implementation only allows add+copy and copy+add instructions.
*/
#define VCD_ADDMAX	256	/* absolute max for a single ADD	*/
#define VCD_COPYMAX	256	/* absolute max for a single COPY	*/
#define VCD_ADDCOPY	16	/* absolute max for a merged ADD	*/
#define VCD_COPYADD	16	/* absolute max for a merged COPY	*/

typedef unsigned int		Vcdkey_t;
typedef struct _vcdlink_s	Vcdlink_t;
typedef struct _vcdiff_s	Vcdiff_t;

typedef struct _vcdcache_s	Vcdcache_t;
typedef struct _vcdsave_s	Vcdsave_t;

typedef struct _vcdsize_s	Vcdsize_t;
typedef struct _vcdindex_s	Vcdindex_t;

/* The next two structs define tables used to compute
   merged ADD+COPY and COPY+ADD instructions.
*/
struct _vcdsize_s
{	int		add;			/* max single ADD size	*/
	int		copy[VCD_ADDR];		/* max single COPY size	*/
	int		add1[VCD_ADDR];		/* max ADD size in A+C	*/
	int		copy2[VCD_ADDR];	/* max COPY size in A+C	*/
	int		copy1[VCD_ADDR];	/* max COPY size in C+A	*/
	int		add2[VCD_ADDR];		/* max ADD size in C+A	*/
};

struct _vcdindex_s
{	Vcchar_t	add[VCD_ADDMAX];
	Vcchar_t	copy[VCD_ADDR][VCD_COPYMAX];
	Vcchar_t	addcopy[VCD_ADDCOPY][VCD_ADDR][VCD_COPYADD];
	Vcchar_t	copyadd[VCD_ADDR][VCD_COPYADD][VCD_ADDCOPY];
};

/* element in search hash table */
struct _vcdlink_s
{	Vcdlink_t*	next;
};

/* address caches to code COPY addresses */
struct _vcdcache_s
{
	int		s_near;		/* size of near cache		*/
	int*		near;		/* near address cache		*/
	int		n;		/* index in near cache		*/
	int		s_same;		/* size of same cache		*/
	int*		same;		/* exact match cache		*/
};

/* to store a saved instruction */
struct _vcdsave_s
{	Vcchar_t*	data;		/* data if an ADD instruction	*/
	int		size;		/* size of ADD or COPY		*/
	int		mode;		/* address encoding mode	*/
	int		best;		/* the best address to code	*/
	int		here;		/* current location		*/
};

/* the Vcdiff/undiff handle */
struct _vcdiff_s
{	Vcdcache_t*	cache;		/* address caches		*/
	Vcdtable_t*	table;		/* code table for compression	*/

	Vcdsize_t*	size;
	Vcdindex_t*	index;

	Vcodex_t*	vcdata;		/* unmatched data processor	*/
	Vcodex_t*	vcinst;		/* instruction processor	*/
	Vcodex_t*	vcaddr;		/* COPY address processor	*/

	Vcchar_t*	src;		/* source string		*/
	size_t		nsrc;		/* length of source string	*/
	Vcchar_t*	tar;		/* target string		*/
	size_t		ntar;		/* length of target string	*/

	Void_t*		dbase;		/* unmatched data		*/
	Void_t*		dnext;		/* next in stream		*/
	Void_t*		ibase;		/* instruction data		*/
	Void_t*		inext;		/* next in stream		*/
	Void_t*		abase;		/* COPY address data		*/
	Void_t*		anext;		/* next in stream		*/

	Vcdsave_t*	save;		/* saved instruction		*/

	Vcdlink_t*	lsrc;		/* links for source string	*/
	Vcdlink_t*	ltar;		/* links for target string	*/
	Vcdlink_t**	htab;		/* hash table			*/
	Vcdkey_t	hmsk;		/* mask for indexing into htab	*/

	int		flags;		/* state flags			*/
};

#define VCDINIT(vcd) \
	( (vcd)->cache = NIL(Vcdcache_t*), \
	  (vcd)->table = NIL(Vcdtable_t*), \
	  (vcd)->size = NIL(Vcdsize_t*), \
	  (vcd)->index = NIL(Vcdindex_t*), \
	  (vcd)->vcdata = NIL(Vcodex_t*), \
	  (vcd)->vcinst = NIL(Vcodex_t*), \
	  (vcd)->vcaddr = NIL(Vcodex_t*), \
	  (vcd)->src = NIL(Vcchar_t*), \
	  (vcd)->nsrc = 0, \
	  (vcd)->tar = NIL(Vcchar_t*), \
	  (vcd)->ntar = 0, \
	  (vcd)->dbase = NIL(Void_t*), \
	  (vcd)->dnext = NIL(Void_t*), \
	  (vcd)->ibase = NIL(Void_t*), \
	  (vcd)->inext = NIL(Void_t*), \
	  (vcd)->abase = NIL(Void_t*), \
	  (vcd)->anext = NIL(Void_t*), \
	  (vcd)->save = NIL(Vcdsave_t*), \
	  (vcd)->lsrc = NIL(Vcdlink_t*), \
	  (vcd)->ltar = NIL(Vcdlink_t*), \
	  (vcd)->htab = NIL(Vcdlink_t**), \
	  (vcd)->hmsk = 0, \
	  (vcd)->flags = 0 \
	)

#define VCD_DELTAONLY	01	/* delta only, no self-compression	*/

_BEGIN_EXTERNS_ /* private functions */
extern Vcdsize_t	_Vcdsize;
extern Vcdindex_t	_Vcdindex;
extern Vcdtable_t*	_Vcdtbl;

extern Vcdcache_t*	vcdkaopen _ARG_((int, int));
extern void		vcdkaclear _ARG_((Vcdcache_t*));
extern void		vcdkaclose _ARG_((Vcdcache_t*));
extern int		vcdkasetaddr _ARG_((Vcdcache_t*, int, int, int*));
extern int		vcdkagetaddr _ARG_((Vcdcache_t*, Vcchar_t**, int, int));
extern void            _vcdtblinit _ARG_(());
_END_EXTERNS_

#endif /*_VDELHDR_H*/
