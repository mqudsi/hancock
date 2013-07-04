# Build programs to generate test/data for tests/suite
#
#
SHELL=/bin/sh

## location of hcc executable.
HCC=../../bin/hcc


INCLUDES=
FLAGS=$(INCLUDES) -g

FILES = a2b b2a gen_grabABag gen_grabBag gen_intPair gen_active

all : $(FILES) 

pre_data:
	rm -rf data
	mkdir data

data: pre_data grabABag.bin grabBag.bin intPair.bin active.bin a2b
	cp rawData/ints.ascii data
	cp rawData/chars.ascii data
	cp rawData/keys data
	mkdir data/chars
	cp rawData/chars/g.ascii data/chars
	cp rawData/chars/h.ascii data/chars
	./a2b < rawData/calls10.a > data/calls10

gen_active: gen_active.hc
	$(HCC) -o gen_active $(FLAGS) gen_active.hc

active.bin: gen_active
	./gen_active

clean:	
	rm -f *~
	rm -f $(addsuffix .o, $(FILES))
	rm -f $(FILES)
	rm -rf data
	rm -rf log
	rm -rf gen_active.c

% : %.c
	$(HCC) -o $@ $(FLAGS) $<

%.bin : gen_%
	./$< > data/$@

%.exe: %.hc
	$(HCC) -o $@ $(FLAGS) $<

