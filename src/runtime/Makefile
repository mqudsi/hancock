SHELL=/bin/sh

.PHONY: SFIO HRS hrs-clean sfio-clean

all:	SFIO HRS

clean:	sfio-clean hrs-clean

SFIO:
	cd sfio; $(MAKE) -f Makefile CC="$(CC)" CXFLAGS="$(CXFLAGS)" AR="$(AR)"; cd ..

sfio-clean:
	cd sfio; $(MAKE) -f Makefile CC="$(CC)" CXFLAGS="$(CXFLAGS)" AR="$(AR)" clean; cd ..

HRS:
	cd hrs/src; \
	$(MAKE) -f Makefile CC="$(CC)" CXFLAGS="$(CXFLAGS)" CMODE="-g" AR="$(AR)"; \
	cp libhrs.a ../lib/libhrs-g.a ; \
	$(MAKE) -f Makefile CC="$(CC)" CXFLAGS="$(CXFLAGS)" AR="$(AR)"; \
	cp libhrs.a ../lib/libhrs.a

hrs-clean:	
	cd hrs/src; $(MAKE) -f Makefile CC="$(CC)" CXFLAGS="$(CXFLAGS)" clean; cd ../..
	cd hrs/lib; rm -rf libhrs-g.a


