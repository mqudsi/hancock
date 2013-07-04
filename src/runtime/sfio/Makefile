Shell=/bin/sh

all:	
	cd src/lib/sfio; $(MAKE) -f Makefile CC="$(CC)" CXFLAGS="$(CXFLAGS)" AR="$(AR)" ; cd ../../..
	cd src/lib/vcodex; $(MAKE) -f Makefile CC="$(CC)" CXFLAGS="$(CXFLAGS)" AR="$(AR)" ; cd ../../..

clean:
	cd src/lib/sfio; $(MAKE) -f Makefile CC="$(CC)" CXFLAGS="$(CXFLAGS)" AR="$(AR)" clean; cd ../../.. ; \
	cd src/lib/vcodex; $(MAKE) -f Makefile CC="$(CC)" CXFLAGS="$(CXFLAGS)" AR="$(AR)" clean; cd ../../.. ; \
	cd include; rm -rf ast_common.h sfdisc.h sfio.h sfio_t.h stdio.h vcdiff.h vcodex.h; cd ..; \
	cd lib; rm -rf libsfio.a libstdio-mt.a libstdio.a libvcodex.a libvthread.a

