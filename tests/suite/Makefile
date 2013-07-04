SHELL= /bin/sh

# Hancock test programs
#
#
###############################################################################
# Default environment variables.
###############################################################################

## root hancock directory.

## flags to always pass to the hancock compiler
HCC_FLAGS=  -t

## location of hcc executable.
HCC=../../bin/hcc

###############################################################################
# Hancock Variables (should not need to change)
###############################################################################

DATAMAKEDIR= ../genData
DATADIR= $(DATAMAKEDIR)/data

ALLDEPEND=

REDIRECT= >> test.out 2>>test.out

SIZE_CMD = $(DATAMAKEDIR)/size_cmd

###############################################################################
#  Test Data
###############################################################################
TEST_MAP       = $(DATADIR)/testMap
TEST_DIR       = $(DATADIR)/testDir
SMALL_STREAM   = $(DATADIR)/calls10
ACTIVE_MAP     = $(DATADIR)/activeMap
ACTIVE_MAP_DC  = $(DATADIR)/activeMapDC
ACTIVE_MAP_NEW = $(DATADIR)/activeMapNew
TEST_PICKLE    = $(DATADIR)/testPickle
HL_TEST_MAP    = $(DATADIR)/hl.testMap
HL_KEYS        = $(DATADIR)/keys

## Removed before each run
TEMP_DATA = $(TEST_DIR) $(TEST_DIR)2 $(TEST_MAP) $(ACTIVE_MAP_NEW) newMap cacheP mapDir myDir1 myDir2 newMap1 newMap2 

###############################################################################
#  Test programs.
###############################################################################

SIGMAIN= sigmain001.tst sigmain002.tst sigmain003.tst sigmain004.tst sigmain005.tst sigmain006.tst sigmain007.tst sigmain008.tst sigmain009.tst sigmain010.tst sigmain011.tst sigmain012.tst sigmain014.tst sigmain015.tst sigmain016.tst
MAP=  map002.tst map003.tst map004.tst map005.tst map006.tst map007.tst map008.tst map009.tst
RECORD=record001.tst record002.tst record003.tst record004.tst record005.tst record006.tst
RANGE= range002.tst range006.tst range007.tst
PHASE=phase001.tst phase002.tst phase003.tst phase004.tst phase005.tst phase006.tst phase008.tst phase009.tst
ITERATION=iteration001.tst iteration002.tst iteration003.tst iteration004.tst iteration005.tst iteration007.tst iteration008.tst iteration009.tst iteration010.tst


DIRECTORY=directory001.tst directory002.tst directory003.tst directory004.tst directory005.tst directory006.tst directory007.tst directory008.tst directory009.tst directory010.tst directory011.tst directory013.tst directory014.tst directory015.tst directory016.tst directory017.tst
STREAM= stream001.tst stream002.tst stream003.tst
SORT= sort001.tst sort002.tst sort003.tst sort004.tst sort005.tst sort006.tst
MEMBER=member001.tst member002.tst
SWITCHES=switches001.tst switches002.tst
INIT=init001.tst
PICKLE=pickle001.tst pickle002.tst
PARAM=param001.tst param002.tst param003.tst param004.tst param005.tst param006.tst param007.tst param008.tst param009.tst

HL=hl001.tst hl002.tst hl003.tst hl004.tst

# $(RANGE) $(PHASE) $(ITERATION) 
SIGS = $(SIGMAIN) $(MAP) $(RECORD) $(RANGE) $(PHASE) $(ITERATION) $(DIRECTORY) $(STREAM) $(SORT) $(MEMBER) $(SWITCHES) $(INIT) $(PICKLE) $(PARAM) $(HL)

INSTALL_SUITE=  sigmain004.tst map007.tst iteration001.tst directory008.tst directory009.tst stream002.tst sort005.tst pickle001.tst param009.tst hl001.tst

ALLSIGS = $(SIGS)

###############################################################################
#  Targets
###############################################################################

all: run

test_suite:	$(SIGS)

install_suite:	$(INSTALL_SUITE)

run_time:	
	echo Running test suite, please wait.  See log file for status.
	time $(MAKE) test_suite >>log 2>>log; 
	d=`diff test.out sample.out`; if [ "$$d" ] ; \
           then echo "Test suite failed. Differences:" ; echo "$$d" ;    \
	   else echo "Test suite successful."; fi

install_time:
	@echo Running installation test, please wait.  See log file for status.
	time $(MAKE) install_suite >>log 2>>log; 
	@d=`diff test.out install.out`; if [ "$$d" ] ; \
           then echo "Installation failed. Differences:" ; echo "$$d" ;    \
	   else $(MAKE) quiet_clean >>log 2>>log ; echo "Installation successful."; fi

run: clean_data clean_log datadir run_time

install_check:	clean_data datadir install_time

datadir:
	@echo Making test data.
	cd $(DATAMAKEDIR); $(MAKE) data >>log 2>>log

clean_data:
	@echo Removing old data and test.out.
	rm -rf $(TEMP_DATA)
	rm -f test.out

clean: clean_data
	rm -f $(SIGS:.tst=.c)
	rm -rf *.hp *.hpp *.ha *.exe *~

quiet_clean: clean_data
	rm -f $(SIGS:.tst=.c)
	rm -rf *.hp *.hpp *.ha *.exe *~
	cd $(DATAMAKEDIR) && $(MAKE) clean

clean_log:
	rm -rf log

very_clean: clean
	cd $(DATAMAKEDIR) && $(MAKE) clean


sigmain001.tst:
	$(HCC) -t sigmain001.hc -o sigmain001.exe
	echo = sigmain001.exe = $(REDIRECT)
	-./sigmain001.exe $(REDIRECT)
	-./sigmain001.exe -t a -u -v -w -x b $(REDIRECT)
	-./sigmain001.exe -x t $(REDIRECT)
	-./sigmain001.exe -x "hello" $(REDIRECT)

sigmain002.tst:
	$(HCC) sigmain002.hc -o sigmain002.exe
	echo = sigmain002.exe = $(REDIRECT)
	-./sigmain002.exe -s "hi" $(REDIRECT)
	-./sigmain002.exe -s "hi" -f "goodbye" $(REDIRECT)
	-./sigmain002.exe -f "goodbye" $(REDIRECT)

sigmain003.tst:
	$(HCC) sigmain003.hc -o sigmain003.exe
	echo = sigmain003.exe = $(REDIRECT)
	-./sigmain003.exe $(REDIRECT)
	-./sigmain003.exe -t 0 -u 0 $(REDIRECT)
	-./sigmain003.exe -t $(SMALL_STREAM) -u $(SMALL_STREAM) $(REDIRECT)
	-./sigmain003.exe -f $(SMALL_STREAM) -t $(SMALL_STREAM) -u $(SMALL_STREAM) $(REDIRECT)

sigmain004.tst:
	$(HCC) sigmain004.hc -o sigmain004.exe
	echo = sigmain004.exe = $(REDIRECT)
	-./sigmain004.exe $(REDIRECT)
	-./sigmain004.exe -t $(ACTIVE_MAP) -u $(ACTIVE_MAP_DC) -v $(ACTIVE_MAP_NEW) -w $(ACTIVE_MAP_DC) -x $(ACTIVE_MAP) $(REDIRECT)
	-rm -f $(ACTIVE_MAP_NEW)
	-./sigmain004.exe -t $(ACTIVE_MAP) -u $(ACTIVE_MAP_DC) -v $(ACTIVE_MAP_NEW) -w $(ACTIVE_MAP_NEW)2 -x $(ACTIVE_MAP) $(REDIRECT)
	-./sigmain004.exe -t $(ACTIVE_MAP) -u $(ACTIVE_MAP_DC) -v $(ACTIVE_MAP) -w $(ACTIVE_MAP_NEW) -x $(ACTIVE_MAP) $(REDIRECT)
	-rm -f $(ACTIVE_MAP_NEW)
	-./sigmain004.exe -t $(ACTIVE_MAP) -u $(ACTIVE_MAP_DC) -v $(ACTIVE_MAP_DC) -w $(ACTIVE_MAP_DC) -x $(ACTIVE_MAP) $(REDIRECT)

sigmain005.tst:
	$(HCC) sigmain005.hc -o sigmain005.exe
	echo = sigmain005.exe = $(REDIRECT)
	-./sigmain005.exe $(REDIRECT)
	-./sigmain005.exe -i 10 $(REDIRECT)
	-./sigmain005.exe -i 11 -k 56 $(REDIRECT)
	-./sigmain005.exe -i -7096 $(REDIRECT)
	-./sigmain005.exe -i 7096788789 $(REDIRECT)
	-./sigmain005.exe -i abc $(REDIRECT)
	-./sigmain005.exe -j 20 $(REDIRECT)

sigmain006.tst:
	$(HCC) sigmain006.hc -o sigmain006.exe
	echo = sigmain006.exe = $(REDIRECT)
	-./sigmain006.exe $(REDIRECT)
	-./sigmain006.exe -k 4.5 $(REDIRECT)
	-./sigmain006.exe -k -4.5 $(REDIRECT)
	-./sigmain006.exe -k 4e4 $(REDIRECT)
	-./sigmain006.exe -k 4w4 $(REDIRECT)

sigmain007.tst:
	$(HCC) sigmain007.hc -o sigmain007.exe
	echo = sigmain007.exe = $(REDIRECT)
	-./sigmain007.exe -v $(TEST_MAP) $(REDIRECT)
	-rm -f $(TEST_MAP)

sigmain008.tst:
	$(HCC) sigmain008.hc  -o sigmain008.exe
	echo = sigmain008.exe = $(REDIRECT)
	-./sigmain008.exe -f $(ACTIVE_MAP) $(REDIRECT)

sigmain009.tst:
	$(HCC) sigmain009.hc -o sigmain009.exe
	echo = sigmain009.exe = $(REDIRECT)
	-./sigmain009.exe -i 123 $(REDIRECT)

sigmain010.tst:
	$(HCC) sigmain010.hc -o sigmain010.exe
	echo = sigmain010.exe = $(REDIRECT)
	-./sigmain010.exe -- $(REDIRECT)

sigmain011.tst:
	$(HCC) sigmain011.hc -o sigmain011.exe
	echo = sigmain011.exe = $(REDIRECT)
	-./sigmain011.exe -f 123.34 $(REDIRECT)

sigmain012.tst:
	$(HCC) sigmain012.hc -o sigmain012.exe
	echo = sigmain012.exe = $(REDIRECT)
	-./sigmain012.exe -l 1231231234 -k 2342342345 -j 2342342345 -i 2342342345 $(REDIRECT)
	-./sigmain012.exe -l -1231231234 -k -2342342345 -j 0 -i 0 $(REDIRECT)
	-./sigmain012.exe -l 1231231234 -k 2342342345 -j -12 -i 0 $(REDIRECT)
	-./sigmain012.exe -l 1231231234 -k 2342342345 -j 12 -i -190 $(REDIRECT)
	-./sigmain012.exe -l 99999999999999999992231231234 -k 2342342345 -j 12 -i 190 $(REDIRECT)

sigmain013.tst:
	$(HCC) sigmain013.hc -o sigmain013.exe
	echo = sigmain013.exe = $(REDIRECT)
	-./sigmain013.exe -l 123.34 $(REDIRECT)

sigmain014.tst:
	$(HCC) sigmain014.hc -o sigmain014.exe
	echo = sigmain014.exe = $(REDIRECT)
	-./sigmain014.exe -u 65535 -s 32767 -j 4294967295 -i 2147483647 $(REDIRECT)
	-./sigmain014.exe -u 65535 -s -32767 -j 4294967295 -i -2147483647 $(REDIRECT)
	-./sigmain014.exe -u -1 -s -32767 -j 4294967295 -i -2147483647 $(REDIRECT)
	-./sigmain014.exe -u 10 -s 32769 -j 4294967295 -i -2147483647 $(REDIRECT)
	-./sigmain014.exe -u 10 -s 32767 -j -4294967295 -i -2147483647 $(REDIRECT)
	-./sigmain014.exe -u 10 -s 32767 -j 4294967295 -i 9147483647 $(REDIRECT)

sigmain015.tst:
	$(HCC) sigmain015.hc -o sigmain015.exe
	echo = sigmain015.exe = $(REDIRECT)
	-./sigmain015.exe -c -124 -s 'a' $(REDIRECT)
	-./sigmain015.exe -c 'b' -s 127 $(REDIRECT)
	-./sigmain015.exe -c 'b' -s 128 $(REDIRECT)
	-./sigmain015.exe -c 255 -s 127 $(REDIRECT)
	-./sigmain015.exe -c 256 -s 127 $(REDIRECT)
	-./sigmain015.exe -c '\165' -s '\n' $(REDIRECT)
	-./sigmain015.exe -c '\xe4' -s '\?' $(REDIRECT)
	-./sigmain015.exe -c '\97' -s '\t' $(REDIRECT)

sigmain016.tst:
	$(HCC) sigmain016.hc -o sigmain016.exe
	echo = sigmain016.exe = $(REDIRECT)
	-./sigmain016.exe -m $(TEST_MAP) $(REDIRECT)
	-rm -f $(TEST_MAP)


map001.tst:
	$(HCC) map001.hc -o map001.exe
	echo = map001.exe = $(REDIRECT)
	-./map001.exe -a $(TEST_MAP) $(REDIRECT)

map002.tst:
	$(HCC) map002.hc -o map002.exe
	echo = map002.exe = $(REDIRECT)
	-./map002.exe -a $(TEST_MAP) $(REDIRECT)
	-rm -f $(TEST_MAP)

map003.tst:
	$(HCC) map003.hc -o map003.exe
	echo = map003.exe = $(REDIRECT)
	-./map003.exe -a $(TEST_MAP) $(REDIRECT)
	-rm -f $(TEST_MAP)

map004.tst:
	$(HCC) map004.hc -o map004.exe
	echo = map004.exe = $(REDIRECT)
	-./map004.exe -a $(TEST_MAP) $(REDIRECT)
	-rm -f $(TEST_MAP)

map005.tst:
	$(HCC) map005.hc -o map005.exe
	echo = map005.exe = $(REDIRECT)

map006.tst:
	$(HCC) map006.hc -o map006.exe
	echo = map006.exe = $(REDIRECT)
	-./map006.exe -d $(TEST_DIR) $(REDIRECT)
	-$(SIZE_CMD) `ls -A $(TEST_DIR)/*` $(REDIRECT)
	-cat $(TEST_DIR)/myVal $(REDIRECT)
	-rm -rf $(TEST_DIR)

map007.tst:
	$(HCC) map007.hc -o map007.exe
	echo = map007.exe = $(REDIRECT)
	-./map007.exe -m $(TEST_MAP) $(REDIRECT)
	-./map007.exe -m $(TEST_MAP) $(REDIRECT)
	-rm -rf $(TEST_MAP)

map008.tst:
	$(HCC) map008.hc -o map008.exe
	echo = map008.exe = $(REDIRECT)
	-./map008.exe -m $(TEST_MAP) $(REDIRECT)
	-./map008.exe -m $(TEST_MAP) $(REDIRECT)
	-rm -rf $(TEST_MAP)

map009.tst:
	$(HCC) map009.hc -o map009.exe
	echo = map009.exe = $(REDIRECT)
	-./map009.exe -m $(TEST_MAP) $(REDIRECT)
	-rm -rf $(TEST_MAP)


phase001.tst:
	$(HCC) phase001.hc -o phase001.exe
	echo = phase001.exe = $(REDIRECT)
	-./phase001.exe -o $(SMALL_STREAM) $(REDIRECT)

phase002.tst:
	$(HCC) phase002.hc -o phase002.exe
	echo = phase002.exe = $(REDIRECT)
	-./phase002.exe -o $(SMALL_STREAM) $(REDIRECT)

phase003.tst:
	$(HCC) phase003.hc -o phase003.exe
	echo = phase003.exe = $(REDIRECT)
	-./phase003.exe -o $(SMALL_STREAM) $(REDIRECT)

phase004.tst:
	$(HCC) phase004.hc -o phase004.exe
	echo = phase004.exe = $(REDIRECT)
	-./phase004.exe -o $(SMALL_STREAM) $(REDIRECT)

phase005.tst:
	$(HCC) phase005.hc -o phase005.exe
	echo = phase005.exe = $(REDIRECT)
	-./phase005.exe -o $(SMALL_STREAM) $(REDIRECT)

phase006.tst:
	$(HCC) phase006.hc -o phase006.exe
	echo = phase006.exe = $(REDIRECT)
	-./phase006.exe -o $(SMALL_STREAM) $(REDIRECT)

phase008.tst:
	$(HCC) phase008.hc -o phase008.exe
	echo = phase008.exe = $(REDIRECT)
	-./phase008.exe -o $(SMALL_STREAM) $(REDIRECT)

phase009.tst: 
	$(HCC) phase009.hc -o phase009.exe
	echo = phase009.exe = $(REDIRECT)
	-./phase009.exe -s $(SMALL_STREAM) $(REDIRECT)

range002.tst:
	$(HCC) range002.hc -o range002.exe
	echo = range002.exe = $(REDIRECT)
	-./range002.exe $(REDIRECT)

range006.tst:
	$(HCC) range006.hc -o range006.exe
	echo = range006.exe = $(REDIRECT)
	-./range006.exe -v $(TEST_MAP) $(REDIRECT)
	-rm -f $(TEST_MAP)

range007.tst:
	$(HCC) range007.hc -o range007.exe
	echo = range007.exe = $(REDIRECT)
	-./range007.exe -v $(TEST_MAP) $(REDIRECT)
	-rm -f $(TEST_MAP)

iteration001.tst:
	$(HCC) iteration001.hc -o iteration001.exe
	echo = iteration001.exe = $(REDIRECT)
	-./iteration001.exe -M $(TEST_MAP) $(REDIRECT)

iteration002.tst:
	$(HCC) iteration002.hc -o iteration002.exe
	echo = iteration002.exe = $(REDIRECT)
	-./iteration002.exe -M $(TEST_MAP) $(REDIRECT)

iteration003.tst:
	$(HCC) iteration003.hc -o iteration003.exe
	echo = iteration003.exe = $(REDIRECT)
	-./iteration003.exe -M $(TEST_MAP) $(REDIRECT)

iteration004.tst:
	$(HCC) iteration004.hc -o iteration004.exe
	echo = iteration004.exe = $(REDIRECT)
	-./iteration004.exe -M $(TEST_MAP) $(REDIRECT)

iteration005.tst:
	$(HCC) iteration005.hc -o iteration005.exe
	echo = iteration005.exe = $(REDIRECT)
	-./iteration005.exe -M $(TEST_MAP) $(REDIRECT)

iteration007.tst:
	$(HCC) iteration007.hc -o iteration007.exe
	echo = iteration007.exe = $(REDIRECT)
	-./iteration007.exe -M $(TEST_MAP) $(REDIRECT)
	-rm -f $(TEST_MAP)

iteration008.tst:
	$(HCC) iteration008.hc -o iteration008.exe
	echo = iteration008.exe = $(REDIRECT)
	-./iteration008.exe $(REDIRECT)

iteration009.tst:
	$(HCC) iteration009.hc -o iteration009.exe
	echo = iteration009.exe = $(REDIRECT)
	-./iteration009.exe -M $(TEST_MAP) $(REDIRECT)
	-rm -f $(TEST_MAP)

iteration010.tst:
	$(HCC) iteration010.hc -o iteration010.exe
	echo = iteration010.exe = $(REDIRECT)
	-./iteration010.exe -M $(TEST_MAP) $(REDIRECT)
	-rm -f $(TEST_MAP)

directory001.tst:
	$(HCC) directory001.hc -o directory001.exe
	echo = directory001.exe = $(REDIRECT)

directory002.tst:
	$(HCC) directory002.hc -o directory002.exe
	echo = directory002.exe = $(REDIRECT)
	-./directory002.exe -d $(TEST_DIR) $(REDIRECT)
	-cat $(TEST_DIR)/* $(REDIRECT)
	-rm -f -r $(TEST_DIR)

directory003.tst:
	$(HCC) directory003.hc -o directory003.exe
	echo = directory003.exe = $(REDIRECT)
	-./directory003.exe -D $(TEST_DIR) $(REDIRECT)
	-$(SIZE_CMD) `ls -A $(TEST_DIR)/*` $(REDIRECT)
	-cat $(TEST_DIR)/myDIData $(REDIRECT)
	-cat $(TEST_DIR)/myDFData $(REDIRECT)

directory004.tst:
	$(HCC) directory004.hc -o directory004.exe
	echo = directory004.exe = $(REDIRECT)
	-./directory004.exe -d $(TEST_DIR) -D $(DATADIR)/testDir2 $(REDIRECT)
	-$(SIZE_CMD) `ls -A $(DATADIR)/testDir2/*` $(REDIRECT)
	-cat $(DATADIR)/testDir2/myDIData $(REDIRECT)
	-cat $(DATADIR)/testDir2/myDFData $(REDIRECT)
	-rm -f -r $(DATADIR)/testDir2
	-rm -f -r $(TEST_DIR)


directory005.tst: 
	$(HCC) directory005.hc -o directory005.exe
	echo = directory005.exe = $(REDIRECT)
	-./directory005.exe -D $(TEST_DIR) $(REDIRECT)
	-$(SIZE_CMD) `ls -A $(TEST_DIR)/*` $(REDIRECT)
	-cat $(TEST_DIR)/myDate $(REDIRECT)
	-cat $(TEST_DIR)/myFloat $(REDIRECT)
	-cat $(TEST_DIR)/myFoo $(REDIRECT)

directory006.tst:
	$(HCC) directory006.hc -o directory006.exe
	echo = directory006.exe = $(REDIRECT)
	-./directory006.exe -d $(TEST_DIR) -D $(DATADIR)/testDir2 $(REDIRECT)
	-$(SIZE_CMD) `ls -A $(DATADIR)/testDir2/*` $(REDIRECT)
	-cat $(DATADIR)/testDir2/myDate $(REDIRECT)
	-cat $(DATADIR)/testDir2/myFloat $(REDIRECT)
	-cat $(DATADIR)/testDir2/myFoo $(REDIRECT)
	-rm -f -r $(DATADIR)/testDir2
	-rm -f -r $(TEST_DIR)

directory007.tst:
	$(HCC) directory007.hc -o directory007.exe
	echo = directory007.exe = $(REDIRECT)
	-./directory007.exe -D $(TEST_DIR) $(REDIRECT)
	-$(SIZE_CMD) `ls -A $(TEST_DIR)/*` $(REDIRECT)
	-cat $(TEST_DIR)/myDate $(REDIRECT)
	-cat $(TEST_DIR)/myFloat $(REDIRECT)
	-cat $(TEST_DIR)/myStruct $(REDIRECT)
	-cat $(TEST_DIR)/myArray $(REDIRECT)
	-rm -f -r $(TEST_DIR)

directory008.tst:
	$(HCC) directory008.hc -o directory008.exe
	echo = directory008.exe = $(REDIRECT)
	-./directory008.exe -D $(TEST_DIR) $(REDIRECT)
	-$(SIZE_CMD) `ls -A $(TEST_DIR)/*` $(REDIRECT)
	-cat $(TEST_DIR)/myDate $(REDIRECT)
	-cat $(TEST_DIR)/myFloat $(REDIRECT)
	-cat $(TEST_DIR)/myChar $(REDIRECT)
	-cat $(TEST_DIR)/myStruct $(REDIRECT)
	-cat $(TEST_DIR)/mySArray $(REDIRECT)

directory009.tst:
	$(HCC) directory009.hc -o directory009.exe
	echo = directory009.exe = $(REDIRECT)
	-./directory009.exe -d $(TEST_DIR) -D $(DATADIR)/testDir2 $(REDIRECT)
	-$(SIZE_CMD) `ls -A $(DATADIR)/testDir2/*` $(REDIRECT)
	-cat $(DATADIR)/testDir2/myDate $(REDIRECT)
	-cat $(DATADIR)/testDir2/myFloat $(REDIRECT)
	-cat $(DATADIR)/testDir2/myChar $(REDIRECT)
	-cat $(DATADIR)/testDir2/myStruct $(REDIRECT)
	-cat $(DATADIR)/testDir2/mySArray $(REDIRECT)
	-rm -f -r $(DATADIR)/testDir2

directory010.tst:
	$(HCC) directory010.hc -o directory010.exe
	echo = directory010.exe = $(REDIRECT)
	-./directory010.exe -d $(TEST_DIR) -D $(DATADIR)/testDir2 $(REDIRECT)
	-$(SIZE_CMD) `ls -A $(DATADIR)/testDir2/*` $(REDIRECT)
	-cat $(DATADIR)/testDir2/myDate $(REDIRECT)
	-cat $(DATADIR)/testDir2/myFloat $(REDIRECT)
	-cat $(DATADIR)/testDir2/myChar $(REDIRECT)
	-cat $(DATADIR)/testDir2/myStruct $(REDIRECT)
	-cat $(DATADIR)/testDir2/mySArray $(REDIRECT)
	-rm -f -r $(DATADIR)/testDir2
	-rm -f -r $(TEST_DIR)

directory011.tst:
	$(HCC) directory011.hc -o directory011.exe
	echo = directory011.exe = $(REDIRECT)
	-./directory011.exe -D $(TEST_DIR) -s "Command-line string" $(REDIRECT)
	-$(SIZE_CMD) `ls -A $(TEST_DIR)/*` $(REDIRECT)
	-cat $(TEST_DIR)/myString $(REDIRECT)
	-rm -f -r $(TEST_DIR)

directory013.tst:
	$(HCC) directory013.hc -o directory013.exe
	echo = directory013.exe = $(REDIRECT)
	-./directory013.exe -m $(TEST_MAP) -d $(TEST_DIR)2 -D $(TEST_DIR)  $(REDIRECT)
	-$(SIZE_CMD) `ls -A $(TEST_DIR)/*` $(REDIRECT)
	-$(SIZE_CMD) "$(TEST_MAP)" $(REDIRECT)
	-$(SIZE_CMD) `ls -A $(TEST_DIR)2/*`  $(REDIRECT)
	-rm -f -r $(TEST_DIR) $(TEST_DIR)2 $(TEST_MAP)

directory014.tst:
	$(HCC) directory014.hc -o directory014.exe
	echo = directory014.exe = $(REDIRECT)
	-./directory014.exe -D $(TEST_DIR)  $(REDIRECT)
	-cat $(TEST_DIR)/c $(REDIRECT)
	-cat $(TEST_DIR)/sc $(REDIRECT)
	-cat $(TEST_DIR)/s $(REDIRECT)
	-cat $(TEST_DIR)/ss $(REDIRECT)
	-cat $(TEST_DIR)/ui $(REDIRECT)
	-cat $(TEST_DIR)/i $(REDIRECT)
	-cat $(TEST_DIR)/ul $(REDIRECT)
	-cat $(TEST_DIR)/l $(REDIRECT)
	-cat $(TEST_DIR)/ull $(REDIRECT)
	-cat $(TEST_DIR)/ll $(REDIRECT)
	-cat $(TEST_DIR)/f $(REDIRECT)
	-cat $(TEST_DIR)/d $(REDIRECT)

directory015.tst:
	$(HCC) directory015.hc -o directory015.exe
	echo = directory015.exe = $(REDIRECT)
	-./directory015.exe -d $(TEST_DIR)  -D $(TEST_DIR)2 $(REDIRECT)
	-cat $(TEST_DIR)2/c $(REDIRECT)
	-cat $(TEST_DIR)2/sc $(REDIRECT)
	-cat $(TEST_DIR)2/s $(REDIRECT)
	-cat $(TEST_DIR)2/ss $(REDIRECT)
	-cat $(TEST_DIR)2/ui $(REDIRECT)
	-cat $(TEST_DIR)2/i $(REDIRECT)
	-cat $(TEST_DIR)2/ul $(REDIRECT)
	-cat $(TEST_DIR)2/l $(REDIRECT)
	-cat $(TEST_DIR)2/ull $(REDIRECT)
	-cat $(TEST_DIR)2/ll $(REDIRECT)
	-cat $(TEST_DIR)2/f $(REDIRECT)
	-cat $(TEST_DIR)2/d $(REDIRECT)
	-rm -f -r $(TEST_DIR)
	-rm -f -r $(TEST_DIR)2

directory016.tst: 
	$(HCC) directory016.hc -o directory016.exe
	echo = directory016.exe = $(REDIRECT)
	-./directory016.exe -D $(TEST_DIR) $(REDIRECT)
	-cat $(TEST_DIR)/f $(REDIRECT)
	-rm -f -r $(TEST_DIR)

directory017.tst: 
	$(HCC) directory017.hc -o directory017.exe
	echo = directory017.exe = $(REDIRECT)
	-./directory017.exe -d $(TEST_DIR) -D $(TEST_DIR)2 $(REDIRECT)
	-cat $(TEST_DIR)/f $(REDIRECT)
	-cat $(TEST_DIR)2/f $(REDIRECT)
	-rm -f -r $(TEST_DIR)
	-rm -f -r $(TEST_DIR)2


stream001.tst:
	$(HCC) stream001.hc -o stream001.exe
	echo = stream001.exe = $(REDIRECT)
	-./stream001.exe -s $(DATADIR)/chars $(REDIRECT)

stream002.tst:
	$(HCC) stream002.hc -o stream002.exe
	echo = stream002.exe = $(REDIRECT)
	-./stream002.exe -s $(DATADIR)/chars $(REDIRECT)

stream003.tst:
	$(HCC) stream003.hc -o stream003.exe
	echo = stream003.exe = $(REDIRECT)
	-./stream003.exe $(REDIRECT)

sort001.tst:
	$(HCC) sort001.hc -o sort001.exe
	echo = sort001.exe = $(REDIRECT)
	-./sort001.exe -c $(DATADIR)/chars.ascii $(REDIRECT)

sort002.tst:
	$(HCC) sort002.hc -o sort002.exe
	echo = sort002.exe = $(REDIRECT)
	-./sort002.exe -c $(DATADIR)/ints.ascii $(REDIRECT)

sort003.tst:
	$(HCC) sort003.hc -o sort003.exe
	echo = sort003.exe = $(REDIRECT)
	-./sort003.exe -c $(DATADIR)/intPair.bin $(REDIRECT)

sort004.tst:
	$(HCC) sort004.hc -o sort004.exe
	echo = sort004.exe = $(REDIRECT)
	-./sort004.exe -c $(DATADIR)/grabBag.bin $(REDIRECT)

sort005.tst:
	$(HCC) sort005.hc -o sort005.exe
	echo = sort005.exe = $(REDIRECT)
	-./sort005.exe -c $(DATADIR)/grabABag.bin $(REDIRECT)

sort006.tst:
	$(HCC) sort006.hc -o sort006.exe
	echo = sort006.exe = $(REDIRECT)
	-./sort006.exe -c $(DATADIR)/grabBag.bin $(REDIRECT)

member001.tst:
	$(HCC) member001.hc -o member001.exe
	echo = member001.exe = $(REDIRECT)
	-./member001.exe -M $(TEST_MAP) $(REDIRECT)

member002.tst:
	$(HCC) member002.hc -o member002.exe
	echo = member002.exe = $(REDIRECT)
	-./member002.exe -M $(TEST_MAP) $(REDIRECT)
	-rm -f $(TEST_MAP)

switches001.tst:
	$(HCC) switches001.hc -o switches001.exe
	echo = switches001.exe = $(REDIRECT)
	-./switches001.exe --  $(REDIRECT)

switches002.tst:
	$(HCC) switches002.hc -o switches002.exe
	echo = switches002.exe = $(REDIRECT)
	-./switches002.exe --  $(REDIRECT)

record001.tst:
	$(HCC) record001.hc -o record001.exe
	echo = record001.exe = $(REDIRECT)
	-./record001.exe $(REDIRECT)

record002.tst:
	$(HCC) record002.hc -o record002.exe
	echo = record002.exe = $(REDIRECT)
	-./record002.exe $(REDIRECT)

record003.tst:
	$(HCC) record003.hc -o record003.exe
	echo = record003.exe = $(REDIRECT)
	-./record003.exe $(REDIRECT)

record004.tst:
	$(HCC) record004.hc -o record004.exe
	echo = record004.exe = $(REDIRECT)
	-./record004.exe $(REDIRECT)

record005.tst:
	$(HCC) record005.hc -o record005.exe
	echo = record005.exe = $(REDIRECT)
	-./record005.exe $(REDIRECT)

record006.tst:
	$(HCC) record006.hc -o record006.exe
	echo = record006.exe = $(REDIRECT)
	-./record006.exe $(REDIRECT)


init001.tst:
	$(HCC) init001.hc -o init001.exe
	echo = init001.exe = $(REDIRECT)
	-./init001.exe -m $(TEST_MAP) -d $(TEST_DIR) -s $(SMALL_STREAM)  $(REDIRECT)
	-rm -f $(TEST_MAP)
	-cat $(TEST_DIR)/* $(REDIRECT)
	-rm -rf $(TEST_DIR)

pickle001.tst: 
	$(HCC) pickle001.hc -o pickle001.exe
	echo = pickle001.exe = $(REDIRECT)
	-./pickle001.exe -p $(TEST_PICKLE) -q $(TEST_PICKLE)2  $(REDIRECT)
	-./pickle001.exe -p $(TEST_PICKLE) -q $(TEST_PICKLE)2  $(REDIRECT)
	-rm -f $(TEST_PICKLE)
	-rm -f $(TEST_PICKLE)2

pickle002.tst: 
	$(HCC) pickle002.hc -o pickle002.exe
	echo = pickle002.exe = $(REDIRECT)
	-./pickle002.exe -d $(TEST_DIR) -D $(TEST_DIR)2  $(REDIRECT)
	-./pickle002.exe -d $(TEST_DIR) -D $(TEST_DIR)2  $(REDIRECT)
	-rm -rf $(TEST_DIR)
	-rm -rf $(TEST_DIR)2

param001.tst:
	$(HCC) param001.hc -o param001.exe
	echo = param001.exe = $(REDIRECT)
	-./param001.exe -i 10 -p $(TEST_PICKLE)1 -s $(TEST_PICKLE)2  $(REDIRECT)
	-cat $(TEST_PICKLE)1 $(REDIRECT)
	-cat $(TEST_PICKLE)2 $(REDIRECT)
	-./param001.exe -i 10 -p $(TEST_PICKLE)1 -s $(TEST_PICKLE)2  $(REDIRECT)
	-cat $(TEST_PICKLE)1 $(REDIRECT)
	-cat $(TEST_PICKLE)2 $(REDIRECT)
	-rm -rf $(TEST_PICKLE)1
	-rm -rf $(TEST_PICKLE)2

param002.tst:
	$(HCC) param002.hc -o param002.exe
	echo = param002.exe = $(REDIRECT)
	-./param002.exe   $(REDIRECT)
	-cat myDir1/nestedDir/myPickle $(REDIRECT)
	-cat myDir1/g $(REDIRECT)
	-cat myDir1/i $(REDIRECT)
	-cat myDir1/str $(REDIRECT)
	-cat myDir1/s $(REDIRECT)
	-cat myDir1/s2 $(REDIRECT)
	-cat myDir1/arr $(REDIRECT)
	-cat myDir2/nestedDir/myPickle $(REDIRECT)
	-cat myDir2/g $(REDIRECT)
	-cat myDir2/i $(REDIRECT)
	-cat myDir2/str $(REDIRECT)
	-cat myDir2/s $(REDIRECT)
	-cat myDir2/s2 $(REDIRECT)
	-cat myDir2/arr $(REDIRECT)
	-./param002.exe $(REDIRECT)
	-cat myDir1/nestedDir/myPickle $(REDIRECT)
	-cat myDir1/g $(REDIRECT)
	-cat myDir1/i $(REDIRECT)
	-cat myDir1/str $(REDIRECT)
	-cat myDir1/s $(REDIRECT)
	-cat myDir1/s2 $(REDIRECT)
	-cat myDir1/arr $(REDIRECT)
	-cat myDir2/nestedDir/myPickle $(REDIRECT)
	-cat myDir2/g $(REDIRECT)
	-cat myDir2/i $(REDIRECT)
	-cat myDir2/str $(REDIRECT)
	-cat myDir2/s $(REDIRECT)
	-cat myDir2/s2 $(REDIRECT)
	-cat myDir2/arr $(REDIRECT)
	-rm -rf myDir1
	-rm -rf myDir2

param003.tst:
	$(HCC) param003.hc -o param003.exe
	echo = param003.exe = $(REDIRECT)
	-./param003.exe   $(REDIRECT)
	-cat mapDir/c $(REDIRECT)
	-./param003.exe   $(REDIRECT)
	-cat mapDir/c $(REDIRECT)
	-rm -rf mapDir

param004.tst:
	$(HCC) param004.hc -o param004.exe
	echo = param004.exe = $(REDIRECT)
	-./param004.exe   $(REDIRECT)
	-cat mapDir/c $(REDIRECT)
	-./param004.exe   $(REDIRECT)
	-cat mapDir/c $(REDIRECT)
	-rm -rf mapDir

param005.tst:
	$(HCC) param005.hc -o param005.exe
	echo = param005.exe = $(REDIRECT)
	-./param005.exe   $(REDIRECT)
	-cat mapDir/c $(REDIRECT)
	-./param005.exe   $(REDIRECT)
	-cat mapDir/c $(REDIRECT)
	-rm -rf mapDir

param006.tst:
	$(HCC) param006.hc -o param006.exe
	echo = param006.exe = $(REDIRECT)
	-./param006.exe   $(REDIRECT)
	-cat cacheP $(REDIRECT)
	-rm -rf cacheP
	-rm -rf newMap

param007.tst:
	$(HCC) param007a.hc -o param007a.exe
	$(HCC) param007b.hc -o param007b.exe
	echo = param007a.exe = $(REDIRECT)
	-./param007a.exe   $(REDIRECT)
	echo = param007b.exe = $(REDIRECT)
	-./param007b.exe   $(REDIRECT)
	-rm -rf newMap

param008.tst:
	$(HCC) param008.hc -o param008.exe
	echo = param008.exe = $(REDIRECT)
	-./param008.exe   $(REDIRECT)
	-cat cacheP
	-rm -rf newMap1
	-rm -rf newMap2
	-rm -rf cacheP

param009.tst:
	$(HCC) param009.hc -o param009.exe
	echo = param009.exe = $(REDIRECT)
	-./param009.exe   $(REDIRECT)

hl001.tst:
	$(HCC) hl001.hc -o hl001.exe
	echo = hl001.exe = $(REDIRECT)
	-./hl001.exe   $(REDIRECT)
	-rm -rf $(HL_TEST_MAP)*
	$(HCC) hl001a.hc -o hl001a.exe
	echo = hl001a.exe = $(REDIRECT)
	-./hl001a.exe   $(REDIRECT)
	-rm -rf $(HL_TEST_MAP)*

hl002.tst:
	$(HCC) hl002.hc -o hl002.exe
	echo = hl002.exe = $(REDIRECT)
	-./hl002.exe   $(REDIRECT)
	-rm -rf $(HL_TEST_MAP)*

hl003.tst:
	$(HCC) hl003.hc -o hl003.exe
	echo = hl003.exe = $(REDIRECT)
	-./hl003.exe   $(REDIRECT)
	-rm -rf $(HL_TEST_MAP)*

hl004.tst:
	$(HCC) hl004.hc -o hl004.exe
	echo = hl004.exe = $(REDIRECT)
	-./hl004.exe   $(REDIRECT) < $(HL_KEYS)
	-rm -rf $(HL_TEST_MAP)*

 %.c:	%.hc $(ALLDEPEND)
	$(HCC) $(HCC_FLAGS)   -C $<

 %.exe: %.hc $(ALLDEPEND)
	$(HCC) $(HCC_FLAGS) -o $(<:.hc=.exe) $<



