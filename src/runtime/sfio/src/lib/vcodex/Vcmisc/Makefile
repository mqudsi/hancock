#	Makefile for the miscellaneous methods
#	Written by Kiem-Phong Vo

INCDIR=	../../../../include
LIBDIR=	../../../../lib

CXFLAGS=
CCMODE= -O
CCFLAGS= $(CCMODE) $(CXFLAGS) -I. -I.. -I$(INCDIR)
CC=cc

VCMSRCS=	vcmap.c vcrle.c
VCMOBJS=	vcmap.o vcrle.o

.c.o :
	$(CC) -c $(CCFLAGS) $*.c

install:	$(VCMOBJS)
clean :
	-(rm $(VCMOBJS); exit 0) >/dev/null 2>&1
