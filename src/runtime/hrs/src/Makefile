SHELL=/bin/sh
SFIO= ../../sfio
INCLUDES= -I$(SFIO)/include -I../include -I.
CMODE=-O
FLAGS= $(INCLUDES) $(CMODE) $(CXFLAGS)
STUFF= ../include/HRS.h 

OFILES= util.o map.o stream.o dir.o hrs.o fc.o rsort.o pickle.o

all:	libhrs.a

libdir:	
	if test ! -d ../lib ; then mkdir ../lib ; fi

libhrs.a:	$(OFILES) libdir
	rm -rf ar.tmp
	mkdir ar.tmp
	cd ar.tmp; rm -f *; $(AR) -x ../$(SFIO)/lib/libsfio.a; $(AR) -x ../$(SFIO)/lib/libvcodex.a
	rm -f libhrs.a
	$(AR) -r libhrs.a  $(OFILES) ar.tmp/*.o
	rm -rf ar.tmp/*.o
	rm -r ar.tmp
	rm -f *.o


util.o: util.c AUTOCONFIG/autoconfig.h $(STUFF)
	$(CC) -c $(FLAGS) util.c

map.o:	map.c mapport.h AUTOCONFIG/autoconfig.h $(STUFF)
	$(CC) -c $(FLAGS)  map.c

pickle.o:	pickle.c AUTOCONFIG/autoconfig.h $(STUFF)
	$(CC) -c $(FLAGS)  pickle.c


stream.o:	stream.c AUTOCONFIG/autoconfig.h $(STUFF)
	$(CC) -c $(FLAGS) stream.c


dir.o:	dir.c $(STUFF)
	$(CC) -c $(FLAGS) dir.c


hrs.o:	hrs.c $(STUFF)
	$(CC) -c $(FLAGS) hrs.c

fc.o:	fc.c fixcut.h $(STUFF)
	$(CC) -c $(FLAGS) fc.c

rsort.o:	rsort.c	
	$(CC) -c $(CXFLAGS) rsort.c


AUTOCONFIG/autoconfig.h:
	cd AUTOCONFIG; $(MAKE) clean; $(MAKE) -f Makefile CC="$(CC)" CFLAGS="$(CXFLAGS)"; cp autoconfig.h ../../include; cd ..

clean:	
	rm -f *.o libhrs.a
	rm -f *~
	rm -f ../lib/libhrs.a
	rm -f ../include/autoconfig.h
	cd AUTOCONFIG; make clean; cd ..

export:	libhrs.a

