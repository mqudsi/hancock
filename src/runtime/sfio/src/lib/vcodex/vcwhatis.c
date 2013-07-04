#include	"vchdr.h"

/* Given an integer, return the method corresponding to it.
**
** Written by Kiem-Phong Vo.
*/

#if __STD_C
Vcmethod_t* vcwhatis(int meth, int inverse)
#else
Vcmethod_t* vcwhatis(meth, inverse)
int	meth;
int	inverse;
#endif
{
	switch(meth)
	{
	case VC_MTDIFF:		return inverse ? Vcundiff : Vcdiff;
	case VC_MTUNDIFF:	return inverse ? NIL(Vcmethod_t*) : Vcundiff;
	case VC_MTMAP:		return inverse ? NIL(Vcmethod_t*) : Vcmap;
#if KPV_OK
	case VC_MTHUFF:		return inverse ? Vcunhuff : Vchuff;
	case VC_MTUNHUFF:	return inverse ? NIL(Vcmethod_t*) : Vcunhuff;
	case VC_MTSPLAY:	return inverse ? Vcunsplay : Vcsplay;
	case VC_MTUNSPLAY:	return inverse ? NIL(Vcmethod_t*) : Vcunsplay;
	case VC_MTBW:		return inverse ? Vcunbw : Vcbw;
	case VC_MTUNBW:		return inverse ? NIL(Vcmethod_t*) : Vcunbw;
	case VC_MTARITH:	return inverse ? Vcunarith : Vcarith;
	case VC_MTUNARITH:	return inverse ? NIL(Vcmethod_t*) : Vcunarith;
	case VC_MTTREE:		return inverse ? Vcuntree : Vctree;
	case VC_MTUNTREE:	return inverse ? NIL(Vcmethod_t*) : Vcuntree;
#endif
	}

	return NIL(Vcmethod_t*);
}
