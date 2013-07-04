#	Makefile for the Vcdiff/Vcundiff methods
#	Written by Kiem-Phong Vo

INCDIR=	../../../../include
LIBDIR=	../../../../lib

CXFLAGS=
CCMODE= -O
CCFLAGS= $(CCMODE) $(CXFLAGS) -I. -I.. -I$(INCDIR)
CC=cc

VCDHDRS=	vcdiff.h vcdhdr.h
VCDSRCS=	vcddiff.c vcdundiff.c vcdcache.c vcdtblinit.c vcdtblencode.c vcd2gdiff.c
VCDOBJS=	vcddiff.o vcdundiff.o vcdcache.o vcdtblinit.o vcdtblencode.o vcd2gdiff.o

.c.o :
	$(CC) -c $(CCFLAGS) $*.c

install_me:	$(VCDOBJS)
install:	install_me

# 3d update
update:
	cp Makefile $(VCDHDRS) $(VCDSRCS) ...

clean :
	-(rm $(VCDOBJS); exit 0) >/dev/null 2>&1
