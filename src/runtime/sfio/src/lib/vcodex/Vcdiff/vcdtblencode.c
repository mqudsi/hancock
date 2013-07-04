#include	"vcdhdr.h"

/*	Encoding and decoding a code table based on differences to another one.
**	This allows efficient embedding of a code table in the compressed data.
**
**	Written by Kiem-Phong Vo (kpv@research.att.com)
*/

#if __STD_C
static void vcdtbl2str(Vcdtable_t* tbl, Void_t* argstr)
#else
static void vcdtbl2str(tbl, argstr)
Vcdtable_t*	tbl;
Void_t*		argstr;
#endif
{
	int		i;
	Vcchar_t*	str = (Vcchar_t*)argstr;
	Vcdcode_t*	code = tbl->code;

	for(i = 0; i < 256; ++i)
		*str++ = code[i].inst1.type;
	for(i = 0; i < 256; ++i)
		*str++ = code[i].inst2.type;
	for(i = 0; i < 256; ++i)
		*str++ = code[i].inst1.size;
	for(i = 0; i < 256; ++i)
		*str++ = code[i].inst2.size;
	for(i = 0; i < 256; ++i)
		*str++ = code[i].inst1.mode;
	for(i = 0; i < 256; ++i)
		*str++ = code[i].inst2.mode;
}

#if __STD_C
static void vcdstr2tbl(Vcdtable_t* tbl, Void_t* argstr)
#else
static void vcdstr2tbl(tbl, argstr)
Vcdtable_t*	tbl;
Void_t*		argstr;
#endif
{
	int		i;
	Vcchar_t*	str = (Vcchar_t*)argstr;
	Vcdcode_t*	code = tbl->code;

	for(i = 0; i < 256; ++i)
		code[i].inst1.type = *str++;
	for(i = 0; i < 256; ++i)
		code[i].inst2.type = *str++;
	for(i = 0; i < 256; ++i)
		code[i].inst1.size = *str++;
	for(i = 0; i < 256; ++i)
		code[i].inst2.size = *str++;
	for(i = 0; i < 256; ++i)
		code[i].inst1.mode = *str++;
	for(i = 0; i < 256; ++i)
		code[i].inst2.mode = *str++;
}

#if __STD_C
ssize_t vcdtblencode(Vcdtable_t* tbl, Void_t* buf, size_t n)
#else
ssize_t vcdtblencode(tbl, buf, n)
Vcdtable_t*	tbl;
Void_t*		buf;
size_t		n;
#endif
{
	Vcodex_t*	vc;
	ssize_t		rv;
	Vcdisc_t	disc;
	Vcchar_t	srcbuf[VCD_TBLSIZE], tarbuf[VCD_TBLSIZE], *del;

	_vcdtblinit();

	/* convert the standard table to a string */
	vcdtbl2str(_Vcdtbl, srcbuf);
	disc.data = srcbuf;
	disc.size = sizeof(srcbuf);
	disc.eventf = NIL(Vcevent_f);
	if(!(vc = vcopen(&disc, Vcdiff, NIL(Void_t*), 0)) )
		return -1;

	/* convert the new table to a string, then encode it against _Vcdtbl */
	vcdtbl2str(tbl, tarbuf);
	if((rv = vcapply(vc, tarbuf, sizeof(tarbuf), &del, 0)) > 0)
	{	if(rv+2 > n)
			rv = -1;
		else
		{	vcputc(&buf, tbl->s_near);
			vcputc(&buf, tbl->s_same);
			memcpy(buf,del,rv);
			rv += 2;
		}
	}

	vcclose(vc);

	return rv;
}


#if __STD_C
int vcdtbldecode(Vcdtable_t* tbl, Void_t* buf, size_t n)
#else
int vcdtbldecode(tbl, buf, n)
Vcdtable_t*	tar;
Void_t*		buf;
size_t		n;
#endif
{
	Vcodex_t*	vc;
	ssize_t		rv;
	Vcdisc_t	disc;
	Vcchar_t	srcbuf[VCD_TBLSIZE], *tarbuf;

	_vcdtblinit();

	if(n <= 2)
		return -1;

	vcdtbl2str(_Vcdtbl, srcbuf);
	disc.data = srcbuf;
	disc.size = sizeof(srcbuf);
	disc.eventf = NIL(Vcevent_f);
	if(!(vc = vcopen(&disc, Vcundiff, NIL(Void_t*), 0)) )
		return -1;

	tbl->s_near = vcgetc(&buf);
	tbl->s_same = vcgetc(&buf);
	if((rv = vcapply(vc, buf, n-2, &tarbuf, 0)) == VCD_TBLSIZE)
		vcdstr2tbl(tbl, tarbuf);
	else	rv = -1;

	vcclose(vc);

	return rv < 0 ? -1 : 0;
}
