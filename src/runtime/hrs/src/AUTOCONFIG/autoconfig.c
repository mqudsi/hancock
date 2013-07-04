/*
** autoconfig.c
**
**  Created by Christof Fetzer, AT&T Labs Research, 4/5/00
**
**  Last update:    $Date: 2001/03/20 11:56:22 $
**  Updated by:     $Author: amr $ 
**
**  $Log: autoconfig.c,v $
**  Revision 1.4  2001/03/20 11:56:22  amr
**
**
**  Newest version from Christof.  3/20/2001.
**
**  Revision 1.11  2001/03/18 17:03:38  christof
**  Output of revision in autoconfig.h
**
**  Revision 1.10  2001/03/16 14:17:48  christof
**   periodic commit
**
**  Revision 1.9  2001/02/27 12:25:25  christof
**  Added MAX_*, MIN_*
**
**  Revision 1.8  2000/10/12 18:57:12  christof
**  Added additional tests to check consistency of functions.
**
**  Revision 1.7  2000/08/10 22:54:01  christof
**  Ported to MSVC.
**
**  Revision 1.6  2000/08/07 17:29:03  christof
**  Fixed to run on Solaris and HP.
**
**  Revision 1.4  2000/07/27 22:51:16  christof
**  1) New Flag CHAR_SIGNED added.
**  2) Protoype if store_* changed.
**
**  Revision 1.3  2000/07/25 15:57:27  christof
**  Example file added. Makefile fixed.
**
**  Revision 1.2  2000/07/25 14:38:14  christof
**  Code cleanup.
**
**  Revision 1.1  2000/07/18 01:34:29  christof
**  Started new Module "AUTOCONFIG"
**
**  Revision 1.1  2000/05/03 22:37:18  christof
**  Initial Revision
**
**  Revision 1.1  2000/04/05 19:38:59  christof
**  Marshalling added.
**
*/

static char *revision = "$Revision: 1.4 $";
#define STR(C) "" ## C ## ""
#include <stdio.h>
#include "compiler_checks.h"

#ifdef QUIET
    int verbose = 0;
#else
    int verbose = 1;
#endif

void genintegers() {
    LONGLONG_t llcint;
    ULONGLONG_t ullcint;
    int   types[] = { sizeof(signed char), sizeof(short), sizeof(int), sizeof(long) , sizeof(llcint) };
    char* maxsnames[] = { "_CHAR", "_SHORT", "_INT", "_LONG", "_LONGLONG" };
    char* snames[] = { "signed char", "short", "int", "long", LONGLONG_str };
    int   utypes[] = { sizeof(unsigned char), sizeof(unsigned short), sizeof(unsigned int), sizeof(unsigned long) , sizeof(ullcint) };
    char* unames[] = { "unsigned char", "unsigned short", "unsigned int", "unsigned long", ULONGLONG_str };
    char* maxunames[] = { "_UCHAR", "_USHORT", "_UINT", "_ULONG", "_ULONGLONG" };
    
    char* extensions[] = { "", "", "", ExtensionL_str, ExtensionLL_str };
    char* printDlabels[] = { "%d", "%d", "%d", "%ld", "%lld" };
    char* printXlabels[] = { "%x", "%x", "%x", "%lx", "%llx" };
    char* printUlabels[] = { "%u", "%u", "%u", "%lu", "%llu" };
    int   ts = sizeof(types)/sizeof(int);
    int   j = 0;
    int   tab[] = { 8, 16, 32, 64 };
    int   is = sizeof(tab)/sizeof(int);
    int   i = 0;
    
    for (i= 0 ; i <  is && j < ts; ++ i) {   
    	for (; j < ts && types[j]*8 < tab[i]; ++ j )
	    ;
	if (types[j]*8 != tab[i]) {
	    fprintf(stderr, "autoconfig: There seems to be no type that is %d bits long! Please report.\n", tab[i]);
	} else if (types[j] != utypes[j]) {
	    fprintf(stderr, "auticonfig: It seems that sizeof(%s) != sizeof(%s). Please report.\n", snames[j] , unames[j]);
	} else {
	    printf("typedef %s int%d;\n",  snames[j], tab[i]);
	    printf("typedef %s uint%d;\n", unames[j], tab[i]);
	    if (extensions[j][0]) {
	    	printf("#define I%d_C(v) (%s) (v##%s)\n", tab[i], snames[j], extensions[j]);
	    	printf("#define U%d_C(v) (%s) (v##%s)\n", tab[i], unames[j], extensions[j]);
	    } else {
	    	printf("#define I%d_C(v) (%s) (v)\n", tab[i], snames[j]);
	    	printf("#define U%d_C(v) (%s) (v)\n", tab[i], unames[j]);
	    }
	    printf("#define D%d_L \"%s\"\n", tab[i], printDlabels[j]);
	    printf("#define X%d_L \"%s\"\n", tab[i], printXlabels[j]);
	    printf("#define U%d_L \"%s\"\n", tab[i], printUlabels[j]);
	}
    }
    if (i != is) {
    	fprintf(stderr, "autoconfig: Could not generate all types. Please report.\n");
    }
    
/* generate MAX_ MIN_ macros */    
    for (i= 0 ; i < ts; ++ i) {  
    	ULONGLONG_t  v; 
    	char buf[128];
    	
	v = 1;	
    	sprintf(buf, printUlabels[ts-1], (v<<(types[i]*8-1))-1);
    	printf("#define MAX%s ((%s) %s%s)\n",  maxsnames[i], snames[i], buf, extensions[i]);

	v = 1;
    	sprintf(buf, printUlabels[ts-1], (v<<(types[i]*8-1)));
    	printf("#define MIN%s ((%s) -%s%s)\n",  maxsnames[i], snames[i], buf, extensions[i]);

	v = 0xff;	
    	for (j = 1 ; j < types[i] ; ++ j) {
	    v <<= 8;
	    v |= 0xff;
	}
    	sprintf(buf, printUlabels[ts-1], v);
    	printf("#define MAX%s ((%s) %s%s)\n", maxunames[i], unames[i], buf, extensions[i]);

    	printf("#define MIN%s ((%s) 0%s)\n",  maxunames[i], unames[i], extensions[i] );
    }
    printf("#define MAX_INT8	I8_C(0x7f)\n");
    printf("#define MIN_INT8	I8_C(0x80)\n");
    printf("#define MAX_INT16	I16_C(0x7fff)\n");
    printf("#define MIN_INT16	I16_C(0x8000)\n");
    printf("#define MAX_INT32	I32_C(0x7fffffff)\n");
    printf("#define MIN_INT32	I32_C(0x80000000)\n");
    printf("#define MAX_INT64	I64_C(0x7fffffffffffffff)\n");
    printf("#define MIN_INT64	I64_C(0x8000000000000000)\n");

    printf("#define MAX_UINT8	U8_C(0xff)\n");
    printf("#define MIN_UINT8	U8_C(0)\n");
    printf("#define MAX_UINT16	U16_C(0xffff)\n");
    printf("#define MIN_UINT16	U16_C(0)\n");
    printf("#define MAX_UINT32	U32_C(0xffffffff)\n");
    printf("#define MIN_UINT32	U32_C(0)\n");
    printf("#define MAX_UINT64	U64_C(0xffffffffffffffff)\n");
    printf("#define MIN_UINT64	U64_C(0)\n");
}

void genfloats() {
    int   types[] = { sizeof(float), sizeof(double), sizeof(long double) };
    char* names[] ={ "float", "double", "long double" };
    
    int   ts = sizeof(types)/sizeof(int);
    int   j = 0, i = 0;
    int   tab[] = { 32, 64 };
    int   is = sizeof(tab)/sizeof(int);
        
    for (i= 0 ; i <  is && j < ts; ++ i) {
    	for (; j < ts && types[j]*8 < tab[i]; ++ j )
	    ;
	if (types[j]*8 != tab[i]) {
	    fprintf(stderr, "autoconfig: There seems to be no float that is %d bits long!\n", tab[i]);
	} else {
	    printf("typedef %s float%d;\n",  names[j], tab[i]);
	}
	printf("#define F%d_C(v) (%s) (v)\n", tab[i], names[j]);
	printf("#define E%d_L \"%s\"\n", tab[i], "%e");
	printf("#define G%d_L \"%s\"\n", tab[i], "%g");
	printf("#define F%d_L \"%s\"\n", tab[i], "%f");
    }
}

int main(int argc, char** argv) {
    if(verbose) {
    	printf("/*\n\nThe following code was generated by \"%s\".\nDo not modify!\n\n*/\n\n", revision); 
    }
        
    genintegers();
    genfloats();
    printf(	"\n"
		"#undef __BEGIN_DECLS\n"
		"#undef __END_DECLS\n"
		"#ifdef __cplusplus\n"
		"#define __BEGIN_DECLS extern \"C\" {\n"
		"#define __END_DECLS }\n"
		"#else\n"
		"#define __BEGIN_DECLS /* empty */\n"
		"#define __END_DECLS /* empty */\n"
		"#endif\n\n");

    if(verbose) {
    	printf("/*\n\nEnd of code generated by \"%s\" .\n\n*/ \n\n", argv[0]); 
    }
    
    return 0;
}

