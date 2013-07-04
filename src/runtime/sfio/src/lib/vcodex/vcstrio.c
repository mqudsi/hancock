#include	"vchdr.h"

/*	Functions to read/write integers in various portable formats.
**	These are stolen from the Sfio library and modified so that
**	they deal only with memory buffers.
**
**	Written by Kiem-Phong Vo (kpv@research.att.com)
*/

#if __STD_C
ssize_t _vcputu(Void_t** buf, Vcint_t v)
#else
ssize_t _vcputu(buf, v)
Void_t**	buf;
Vcint_t		v;
#endif
{
	int		n;
	Vcchar_t	data[2*sizeof(Vcint_t)];
	Vcchar_t*	code = &data[sizeof(data) - 1];
	Vcchar_t*	ptr = (Vcchar_t*)(*buf);

	*code = v&127;
	while((v >>= 7) > 0)
		*--code = (v&127) | 128;

	switch((n = &data[sizeof(data)] - code) )
	{ default:	memcpy(ptr, code, n); ptr += n; break;
	  case 7 :	*ptr++ = *code++;
	  case 6 :	*ptr++ = *code++;
	  case 5 :	*ptr++ = *code++;
	  case 4 :	*ptr++ = *code++;
	  case 3 :	*ptr++ = *code++;
	  case 2 :	*ptr++ = *code++;
	  case 1 :	*ptr++ = *code++;
	}

	*buf = (Void_t*)ptr;
	return (ssize_t)n;
}

#if __STD_C
Vcint_t _vcgetu(Void_t** buf)
#else
Vcint_t _vcgetu(buf)
Void_t**	buf;
#endif
{
	int		n;
	Vcint_t		v;
	Vcchar_t*	ptr = (Vcchar_t*)(*buf);

	v = (n = *ptr++)&127;
	while(n & 128)
		v = (v << 7) | ((n = *ptr++)&127);

	*buf = (Void_t*)ptr;

	return v;
}

#if __STD_C
ssize_t _vcputm(Void_t** buf, Vcint_t v, Vcint_t max)
#else
ssize_t _vcputm(buf, v, max)
Void_t**	buf;
Vcint_t		v;
Vcint_t		max;
#endif
{
	int		n;
	Vcchar_t	data[sizeof(Vcint_t)];
	Vcchar_t*	code = &data[sizeof(data) - 1];
	Vcchar_t*	ptr = (Vcchar_t*)(*buf);

	*code = v&255;
	while((max >>= 8) > 0)
		*--code = (v >>= 8)&255;

	switch((n = &data[sizeof(data)] - code) )
	{ default:	memcpy(ptr, code, n); ptr += n; break;
	  case 7 :	*ptr++ = *code++;
	  case 6 :	*ptr++ = *code++;
	  case 5 :	*ptr++ = *code++;
	  case 4 :	*ptr++ = *code++;
	  case 3 :	*ptr++ = *code++;
	  case 2 :	*ptr++ = *code++;
	  case 1 :	*ptr++ = *code++;
	}

	*buf = (Void_t*)ptr;

	return (ssize_t)n;
}

#if __STD_C
Vcint_t _vcgetm(Void_t** buf, Vcint_t max)
#else
Vcint_t _vcgetm(buf, max)
Void_t**	buf;
Vcint_t		max;
#endif
{
	Vcint_t		v;
	Vcchar_t*	ptr = (Vcchar_t*)(*buf);

	v = *ptr++;
	while((max >>= 8) > 0)
		v = (v <<= 8) | *ptr++;

	*buf = (Void_t*)ptr;

	return v;
}
