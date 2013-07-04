SHELL= /bin/sh
# Example Hancock programs from manual
#
#
###############################################################################
# Default environment variables.
###############################################################################

# flags to pass to C compiler
CXFLAGS=
# C compiler to use
CC=$(CC)

## root hancock directory.

## flags to always pass to the hancock compiler
HCC_FLAGS= 

## location of hcc executable.
HCC=../bin/hcc

DDIR=data
CDIR=$(DDIR)/calls
EDIR=$(DDIR)/extended
TEMP_DATA= intlMap $(CDIR)/binary $(DDIR)/ip.tmp.out $(EDIR)/extended.tmp.out $(EDIR)/old_pers_data $(EDIR)/new_pers_data

TARGETS= intlCount intlCountCust intlMapView ip wireless wirelessGen
CLEAN= ip.c intlCount.c intlCountCust.c intlMapView.c worldNet.c test.c wirelessGen.c
###############################################################################
# Run programs
###############################################################################

all:	intlCount.tst intlCountCust.tst ip.tst wireless.tst

clean:	
	rm -rf *.hp *.hpp *.ha *.exe *~ *.o
	rm -rf $(TEMP_DATA)
	rm -rf $(DDIR)/a2b
	rm -rf $(CLEAN)

intlCount.exe:	intlCount.hc
	$(HCC) $(HCC_FLAGS) -o intlCount.exe intlCount.hc

intlCountCust.exe:	intlCountCust.hc
	$(HCC) $(HCC_FLAGS) -o intlCountCust.exe intlCountCust.hc

intlMapView.exe:	intlMapView.hc
	$(HCC) $(HCC_FLAGS) -o intlMapView.exe intlMapView.hc

ip.exe:	ip.hc ipRec.hh ihash.c ihash.h
	$(HCC) $(HCC_FLAGS) -o ip.exe ihash.c ip.hc

wirelessGen.exe:	$(ALLDEPEND) pht.hc pht.hh wirelessGen.hc hash.o Makefile ctp.hc ctp.hh ctMap.hc ctMap.hh ctDir.hh
	$(HCC) $(HCC_FLAGS) -o wirelessGen.exe wirelessGen.hc pht.hc hash.o wcd.hc ctMap.hc ctp.hc


wireless.exe:	$(ALLDEPEND) pht.hc pht.hh wireless.hc hash.o Makefile wcd.hh wcd.hc ctp.hc ctp.hh ctMap.hc ctMap.hh ctDir.hh
	$(HCC) $(HCC_FLAGS) -o wireless.exe wireless.hc pht.hc hash.o wcd.hc ctMap.hc ctp.hc


intlCount.tst:	intlCount.exe calls10
	./intlCount.exe -c $(CDIR)/binary/calls10	

intlMapView.tst:	intlMapView.exe calls10
	./intlMapView.exe -c $(CDIR)/binary/calls10	

intlCountCust.tst : intlCountCust.exe intlMapView.exe calls10
	rm -f intlMap
	./intlCountCust.exe -c $(CDIR)/binary/calls10 -I intlMap
	./intlMapView.exe -I intlMap

ip.tst:	ip.exe
	rm -f ip.tmp.out
	./ip.exe -l $(DDIR)/ipAddrs -p $(DDIR)/packets >$(DDIR)/ip.tmp.out
	diff $(DDIR)/ip.tmp.out  $(DDIR)/ip.sample.out

wireless.tst:	wireless.exe wirelessGen.exe
	rm -rf $(EDIR)/old_pers_data
	rm -rf $(EDIR)/new_pers_data
	./wirelessGen.exe -d $(EDIR)/old_pers_data
	./wireless.exe -d $(EDIR)/old_pers_data -D $(EDIR)/new_pers_data -c $(EDIR)/wcd >$(EDIR)/extended.tmp.out
	diff $(EDIR)/extended.tmp.out $(EDIR)/extended.sample.out

calls10:	$(DDIR)/a2b 
	rm -rf $(CDIR)/binary
	mkdir $(CDIR)/binary
	$(DDIR)/a2b < $(CDIR)/ascii/calls10.a > $(CDIR)/binary/calls10

a2b:	$(DDIR)/a2b.c
	cc -o $(DDIR)/a2b $(DDIR)/a2b.c

 %.o:	%.c
	$(CC) $(CXFLAGS) -c -o $(<:.c=.o) $<

%.exe:	%.hc $(ALLDEPEND)
	$(HCC) $(HCC_FLAGS)  -o  $(<:.hc=.exe) $<

