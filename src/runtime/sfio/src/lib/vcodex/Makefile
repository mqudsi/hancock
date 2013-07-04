#	libvcodex.a makefile
#	Written by Kiem-Phong Vo

INCDIR=	../../../include
LIBDIR=	../../../lib
BINDIR=	../../../bin

CXFLAGS=
CCMODE= -O
CCFLAGS= $(CCMODE) $(CXFLAGS) -I.
CC=cc
AR=ar

VCSRCS=		vcopen.c vcclose.c vcstrio.c vcprivate.c vcdisc.c vcwhatis.c
VCOBJS=		vcopen.o vcclose.o vcstrio.o vcprivate.o vcdisc.o vcwhatis.o
VCHDRS=		ast_common.h vcodex.h

VCDOBJS=	Vcdiff/vcddiff.o Vcdiff/vcdundiff.o Vcdiff/vcdcache.o \
		Vcdiff/vcdtblencode.o Vcdiff/vcdtblinit.o Vcdiff/vcd2gdiff.o
VCDHDRS=	Vcdiff/vcdiff.h

VCMOBJS=	Vcmisc/vcmap.o Vcmisc/vcrle.o

OBJS=		$(VCOBJS) $(VCDOBJS) $(VCMOBJS)
HDRS=		$(VCHDRS) $(VCDHDRS)

.PHONY:	vcdiff vcmisc

.c.o :
	$(CC) -c $(CCFLAGS) $*.c

all:	install

libvcodex.a : ast_common.h $(VCOBJS) vcdiff vcmisc
	$(AR) cr libvcodex.a $(OBJS)
	-(ranlib libvcodex.a; exit 0) >/dev/null 2>&1

ast_common.h: features/common
	$(BINDIR)/iffe - set cc $(CC) $(CCMODE) $(CXFLAGS) : run features/common >ast_common.h

vcdiff:
	cd Vcdiff; \
	$(MAKE) SHELL="$(SHELL)" CC="$(CC)" CCMODE="$(CCMODE)" CXFLAGS="$(CXFLAGS)"

vcmisc:
	cd Vcmisc; \
	$(MAKE) SHELL="$(SHELL)" CC="$(CC)" CCMODE="$(CCMODE)" CXFLAGS="$(CXFLAGS)"

install : libvcodex.a
	cp $(HDRS) $(INCDIR)
	cp libvcodex.a $(LIBDIR)
	-(ranlib $(LIBDIR)/libvcodex.a; exit 0) >/dev/null 2>&1

clean :
	-(rm $(OBJS) libvcodex.a ast_common.h; exit 0) >/dev/null 2>&1
