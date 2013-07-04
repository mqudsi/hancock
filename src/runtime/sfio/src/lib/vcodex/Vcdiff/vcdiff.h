#ifndef _VCDIFF_H
#define _VCDIFF_H	1

/*	Public header file for the Vcdiff and Vcundiff methods.
**
**	Written by Kiem-Phong Vo (kpv@research.att.com)
*/

#include	<vcodex.h>

/* header bytes for a file compressed with Vcdiff */
#define		VCD_HEAD1	('V' | (1<<7))
#define		VCD_HEAD2	('C' | (1<<7))
#define		VCD_HEAD3	('D' | (1<<7))
#define		VCD_HEAD4	(0)

/* bits in the starting control byte */
#define		VCD_DECOMPRESS	(1<<0)	/* using a secondary compressor	*/
#define		VCD_CODETABLE	(1<<1)	/* alternative code table	*/
#define		VCD_INITS	(0x3)

/* bits in the window control byte */
#define		VCD_SOURCE	(1<<0)	/* match window in source file	*/
#define		VCD_TARGET	(1<<1)	/* match window in target file	*/

#define		VCD_DATACOMP	(1<<0)	/* compressed unmatched data	*/
#define		VCD_INSTCOMP	(1<<1)	/* compressed instructions	*/
#define		VCD_ADDRCOMP	(1<<2)	/* compressed COPY addrs	*/

/* instruction types */
#define	VCD_NOOP	0	
#define VCD_ADD		1
#define VCD_RUN		2
#define VCD_COPY	3

/* the address modes are limited to 16 (VCD_ADDR). Of these, VCD_SELF
   and VCD_HERE are reserved. The remaining modes are s+n < 16 where
   s*256 is the size of "same address" cache and n is the size of the
   "near address" cache.
*/
#define VCD_ADDR	16		/* max number of address modes	*/
#define VCD_SELF	0		/* addr coded as itself: 0	*/
#define VCD_HERE	1		/* coded using current pos: 1	*/

/* buffer size requirement for encoding/decoding code tables */
#define VCD_TBLSIZE	(6*256 + 64)

typedef struct _vcdinst_s	Vcdinst_t;	/* instruction		*/
typedef struct _vcdcode_s	Vcdcode_t;	/* a pair of insts	*/
typedef struct _vcdtable_s	Vcdtable_t;	/* code table		*/

typedef struct _vcdinit_s	Vcdinit_t;	/* add'l method params	*/

struct _vcdinit_s
{	Vcmethod_t*	compressor;	/* secondary compressor		*/
	Vcdtable_t*	codetable;	/* table to encode insts	*/
	int		deltaonly;	/* Vcdiff: no compression	*/
};

struct _vcdinst_s
{	Vcchar_t	type;	/* COPY, RUN, ADD, NOOP			*/
	Vcchar_t	size;	/* if 0, size coded separately		*/
	Vcchar_t	mode;	/* address mode for COPY		*/
};

struct _vcdcode_s
{	Vcdinst_t	inst1;
	Vcdinst_t	inst2;
};

struct _vcdtable_s
{	Vcchar_t	s_near;		/* size of near address cache	*/
	Vcchar_t	s_same;		/* size of same address cache	*/
	Vcdcode_t	code[256];	/* codes -> instructions	*/
};

_BEGIN_EXTERNS_
extern ssize_t		vcdtblencode _ARG_((Vcdtable_t*, Void_t*, size_t));
extern int		vcdtbldecode _ARG_((Vcdtable_t*, Void_t*, size_t));
_END_EXTERNS_

#endif /*_VCDIFF_H*/
