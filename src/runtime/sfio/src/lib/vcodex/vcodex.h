#ifndef _VCODEX_H
#define _VCODEX_H	1

/********************************************************************************
*	This product contains certain software code or other information	*
*	("AT&T Software") proprietary to AT&T Corp. ("AT&T").			*
*	The AT&T Software is provided to you "AS IS". YOU ASSUME TOTAL		*
*	RESPONSIBILITY AND RISK FOR USE OF THE AT&T SOFTWARE.			*
*	AT&T DOES NOT MAKE, AND EXPRESSLY DISCLAIMS, ANY EXPRESS OR		*
*	IMPLIED WARRANTIES OF ANY KIND WHATSOEVER, INCLUDING,			*
*	WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTABILITY OR	*
*	FITNESS FOR A PARTICULAR PURPOSE, WARRANTIES OF TITLE OR		*
*	NON-INFRINGEMENT OF ANY INTELLECTUAL PROPERTY RIGHTS,			*
*	ANY WARRANTIES ARISING BY USAGE OF TRADE, COURSE OF DEALING OR		*
*	COURSE OF PERFORMANCE, OR ANY WARRANTY THAT THE AT&T SOFTWARE		*
*	IS "ERROR FREE" OR WILL MEET YOUR REQUIREMENTS. 			*
*										*
*	All rights reserved. AT&T is a registered trademark of AT&T Corp.	*
********************************************************************************/


/*	VCODEX = COmpression + DElta + X (encryption, etc.)
**
**	Written by Kiem-Phong Vo (kpv@research.att.com)
*/

#define VC_VERSION	20010531L	/* a fine day	*/

#if _PACKAGE_ast
#include	<ast_std.h>
#else
#include	<ast_common.h>
#endif

typedef unsigned char		Vcchar_t;
typedef _ast_intmax_t		Vcint_t;
typedef struct _vcodex_s	Vcodex_t;
typedef struct _vcdisc_s	Vcdisc_t;
typedef struct _vcmethod_s	Vcmethod_t;
typedef int			(*Vcevent_f)_ARG_((Vcodex_t*, int, Void_t*, Vcdisc_t*));

/* discipline structure: what application supplies */
struct _vcdisc_s
{	Void_t*		data;		/* source or key string		*/
	size_t		size;		/* size of string		*/
	Vcint_t		offset;		/* file offset, if any		*/
	Vcevent_f	eventf;		/* event handler		*/
};

/* method structure: what library provides */
struct _vcmethod_s
{	ssize_t	(*applyf)_ARG_((Vcodex_t*, const Void_t*, size_t, Void_t**, size_t));
	int	(*eventf)_ARG_((Vcodex_t*, int, Void_t*));
	int	method;			/* method name			*/
};

/* Vcodex_t handle structure: to keep states */
struct _vcodex_s
{	Vcmethod_t*	meth;		/* selected coding method 	*/
	Vcdisc_t*	disc;		/* supplied discipline 		*/
#ifdef _VCODEX_PRIVATE
	_VCODEX_PRIVATE
#endif
};

/* flags - unused for now, but may support thread-safety someday */
#define VC_FLAGS	0

/* event types */
#define VC_OPENING	1	/* opening event			*/
#define VC_CLOSING	2	/* closing event			*/
#define VC_ERRINIT	3	/* error during initialization		*/
#define VC_DISC		4	/* changing discipline			*/
#define VC_CORRUPT	5	/* corrupted data stream		*/

/* Method numbers are designed to fit in 8-bits.
** A pair of decoder/encoder [D,E] is numbered as E = D | (1<<7).
*/
#define VC_MTDIFF	1	/* Vcdiff delta compressor		*/
#define VC_MTUNDIFF	(VC_MTDIFF | (1<<7))

#define VC_MTHUFF	2	/* Huffman compressor			*/
#define VC_MTUNHUFF	(VC_MTHUFF | (1<<7))

#define VC_MTSPLAY	3	/* splay tree compressor		*/
#define VC_MTUNSPLAY	(VC_MTSPLAY | (1<<7))

#define VC_MTARITH	4	/* arithmetic encoding			*/
#define VC_MTUNARITH	(VC_MTARITH | (1<<7))

#define VC_MTRLE	50	/* run-length encoder			*/
#define VC_MTUNRLE	(VC_MTRLE | (1<<7))

#define VC_MTBW		51	/* Burrow-Wheeler transform		*/
#define VC_MTUNBW	(VC_MTBW | (1<<7))

#define VC_MTTREE	52	/* tree encryptor			*/
#define VC_MTUNTREE	(VC_MTTREE | (1<<7))

#define VC_MTMAP	100	/* map bytes from one to another	*/
#define VC_MT2GDIFF	101	/* convert vcdiff to gdiff		*/

_BEGIN_EXTERNS_

extern Vcmethod_t*	Vcdiff;		/* compression/differencing	*/
extern Vcmethod_t*	Vcundiff;	/* inverse of Vcdiff		*/
extern Vcmethod_t*	Vcrle;		/* run-length encoder		*/
extern Vcmethod_t*	Vcunrle;	/* run-length decoder		*/
extern Vcmethod_t*	Vcmap;		/* mapping bytes (e.g., rot13)	*/
extern Vcmethod_t*	Vc2gdiff;	/* vcdiff -> gdiff		*/

#ifdef KPV_OK
extern Vcmethod_t*	Vcunhuff;	/* inverse of Vchuff		*/
extern Vcmethod_t*	Vchuff;		/* Huffman compression		*/
extern Vcmethod_t*	Vcsplay;	/* splay tree compression	*/
extern Vcmethod_t*	Vcunsplay;	/* inverse of Vcsplay		*/
extern Vcmethod_t*	Vcarith;	/* arithmetic compression	*/
extern Vcmethod_t*	Vcunarith;	/* inverse of Vcarith		*/
extern Vcmethod_t*	Vcbw;		/* Burrow-Wheeler transform	*/
extern Vcmethod_t*	Vcunbw;		/* inverse of Vcbw		*/

extern Vcmethod_t*	Vctree;		/* compression/encryption	*/
extern Vcmethod_t*	Vcuntree;	/* inverse of Vctree		*/
#endif

extern Vcodex_t*	vcopen _ARG_((Vcdisc_t*, Vcmethod_t*, Void_t*, int));
extern int		vcclose _ARG_((Vcodex_t*));
extern ssize_t		vcapply _ARG_((Vcodex_t*, Void_t*, size_t, Void_t**, size_t));

extern Vcdisc_t*	vcdisc _ARG_((Vcodex_t*, Vcdisc_t*));

extern Vcmethod_t*	vcwhatis _ARG_((int, int));

extern ssize_t		vcputu _ARG_((Void_t**, Vcint_t));
extern Vcint_t		vcgetu _ARG_((Void_t**));
extern ssize_t		vcputm _ARG_((Void_t**, Vcint_t, Vcint_t));
extern Vcint_t		vcgetm _ARG_((Void_t**, Vcint_t));
extern ssize_t		vcputs _ARG_((Void_t**, Void_t*, size_t));
extern ssize_t		vcgets _ARG_((Void_t**, Void_t*, size_t));
extern ssize_t		vcputc _ARG_((Void_t**, int));
extern int		vcgetc _ARG_((Void_t**));

extern ssize_t		_vcputu _ARG_((Void_t**, Vcint_t));
extern Vcint_t		_vcgetu _ARG_((Void_t**));
extern ssize_t		_vcputm _ARG_((Void_t**, Vcint_t, Vcint_t));
extern Vcint_t		_vcgetm _ARG_((Void_t**, Vcint_t));

_END_EXTERNS_

#define _VCBUF(buf)		((Vcchar_t*)(*(buf)))
#define _VCINC(buf,n)		((*((Vcchar_t**)buf) = _VCBUF(buf)+(n)),(ssize_t)(n))
#define vcputc(buf, v)		((*_VCBUF(buf) = (int)(v)), _VCINC(buf,1) )
#define vcgetc(buf)		(_VCINC(buf,1), ((int)(*(_VCBUF(buf)-1))) )
#define vcputs(buf, str, sz)	(memcpy(_VCBUF(buf), (str), (sz)), _VCINC(buf,sz) )
#define vcgets(buf, str, sz)	(memcpy((str), _VCBUF(buf), (sz)), _VCINC(buf,sz) )
#define vcputm(buf, v, m)	_vcputm((Void_t**)(buf), (Vcint_t)(v), (Vcint_t)(m))
#define vcgetm(buf, m)		_vcgetm((Void_t**)(buf), (Vcint_t)(m))
#define vcputu(buf, v)		_vcputu((Void_t**)(buf), (Vcint_t)(v))
#define vcgetu(buf)		_vcgetu((Void_t**)(buf))

#define vcapply(vc,s,sz,b,bz)	(*(vc)->meth->applyf)((vc), (Void_t*)(s), (size_t)(sz), \
					(Void_t**)(b), (size_t)(bz))

/* for public usage, include all definitions for the supplied methods */
#ifndef _VCHDR_H
#include	<vcdiff.h>
#endif

#endif /*_VCODEX_H*/
