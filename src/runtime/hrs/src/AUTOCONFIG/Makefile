all: autoconfig.h autoconfig.tar example test1 test2

clean:
	rm -f test1 test2
	rm -f autoconfig.h
	rm -f autoconfig
	rm -f tmp_autoconfig.h
	rm -f core
	rm -f tmp_config.c
	rm -f a.out
	rm -f autoconfig.tar
	rm -f endian example  compiler_checks.h tmp_config
	rm -f portability.aux portability.log portability.pdf portability.ps portability.dvi

test1: test1.c autoconfig.h
	$(CC) $(CFLAGS) -o test1 test1.c
	./test1
	rm -f test1

test2: test2.c autoconfig.h
	$(CC) $(CFLAGS) -o test2 test2.c
	./test2
	rm -f test2

example: example.c autoconfig.h
	$(CC) $(CFLAGS) -o example example.c
	./example

endian: endian.c tmp_autoconfig.h Makefile
	$(CC) $(CFLAGS) -I. -DREDUCE_WARNINGS -o endian endian.c 

autoconfig.h: endian tmp_autoconfig.h Makefile
	echo "#ifndef __ENDIAN_H__" >> autoconfig.h
	echo "#define __ENDIAN_H__" >> autoconfig.h
	./endian >> autoconfig.h
	echo "#endif /* __ENDIAN_H__ */" >> autoconfig.h

tmp_autoconfig.h: autoconfig endian.c Makefile config.c 
	echo "#ifndef __AUTOCONFIG_H__" > autoconfig.h
	echo "#define __AUTOCONFIG_H__" >> autoconfig.h
	echo "/* README " >> autoconfig.h
	cat README >>autoconfig.h
	echo "END OF README */"  >>autoconfig.h
	./autoconfig >>autoconfig.h
	touch tmp_autoconfig.h
	sh config.c "$(CC) $(CFLAGS)" >>autoconfig.h
	echo "#endif /* __AUTOCONFIG_H__ */" >> autoconfig.h

compiler_checks.h: compiler_checks.c
	sh compiler_checks.c $(CC) $(CFLAGS) >compiler_checks.h

autoconfig: autoconfig.c Makefile compiler_checks.h
	$(CC) $(CFLAGS) -o autoconfig autoconfig.c	

autoconfig.tar: Makefile endian.c autoconfig.c config.c example.c README compiler_checks.c test1.c test2.c
	cd ..; tar -chf AUTOCONFIG/autoconfig.tar AUTOCONFIG/config.c AUTOCONFIG/autoconfig.c AUTOCONFIG/endian.c  AUTOCONFIG/Makefile  AUTOCONFIG/example.c AUTOCONFIG/README AUTOCONFIG/compiler_checks.c  AUTOCONFIG/test1.c  AUTOCONFIG/test2.c

portability.ps: portability.dvi
	dvips portability.dvi

portability.pdf: portability.dvi
	dvipdf portability.dvi

portability.dvi: portability.tex
	latex portability.tex
	latex portability.tex
