## #############################################################
## 
## Special makes -----------------------------------------------
##
## test:      construct a date-dependent sanity test	
##
## millstone: Generate the Millstone Version of the Crypter.
## 
## jar:	      pack the relevant java/class files into pack.jar 
## 
## testsuite: generate + pack files in the testCrypter directory
## 
## -------------------------------------------------------------
## 
## Synopsis:
##  Contains Birthdays, Weddings ans similar
##  To be included in ~/diary
## #############################################################
## @TABLE OF CONTENTS:		       [TOCD: 11:23 20 Nov 2004]
##
##  [1] Java Compile
##  [2] Test
##  [3] Jar
##  [4] testCrypter : a test suite
##  [5] Publish
## ##########################################################
## @FILE:    Makefile
## @PLACE:   Gaia Home
## @FORMAT:  plain text
## @AUTHOR:  M. Oliver M'o'ller     <oliver.moeller@verified.de>
## @BEGUN:   Mon Feb  3 23:10:12 2003
## @VERSION: Mon Feb  3 23:10:12 2003
## #############################################################
## 

TEST_BITS=8

JAVA_FILES=$(wildcard ./*.java)


OBJECT_FILES=$(patsubst ./%.java,./%.class,$(JAVA_FILES))


all:	$(OBJECT_FILES)


clean:
	rm *.class


## ###################################################################
## [1] Java Compile
## ###################################################################

%.class:	%.java
	javac -deprecation $< ;


## ###################################################################
## [2] Test
## ###################################################################

.PHONEY:  test test_ordinary test_binary check

test:	 test_ordinary test_binary check

test_ordinary: $(OBJECT_FILES)
	@if [ ! -d tmp ]; then mkdir tmp; fi ;
	@cp ordinary.in ./tmp/test.in ; 
	@date >> ./tmp/test.in ; 
	java Crypter e -$(TEST_BITS) ./tmp/test.in  ./tmp/test.out ; 
	java Crypter d -$(TEST_BITS) ./tmp/test.out ./tmp/test.decrypted ; 
	@if cmp -c ./tmp/test.in ./tmp/test.decrypted; then \
		echo "** Test successful." ; \
	else \
		echo "** Test FAILED!" ; \
		diff ./tmp/test.in ./tmp/test.decrypted ; \
		exit 7; \
	fi; \

test_binary: $(OBJECT_FILES)
	@if [ ! -d tmp ]; then mkdir tmp; fi ;
	@cp binary.in ./tmp/test.in ; 
	@date >> ./tmp/test.in ; 
	java Crypter e -$(TEST_BITS) ./tmp/test.in  ./tmp/test.out ; 
	java Crypter d -$(TEST_BITS) ./tmp/test.out ./tmp/test.decrypted ; 
	@if cmp -c ./tmp/test.in ./tmp/test.decrypted; then \
		echo "** Test successful." ; \
	else \
		echo "** Test FAILED!" ; \
		diff ./tmp/test.in ./tmp/test.decrypted ; \
		exit 7; \
	fi; \


check:
	cd test ; \
	make check; 


## ###################################################################
## [3] Jar
## ###################################################################

.PHONEY:	jar

jar:	$(OBJECT_FILES)
	\rm pack.jar ; \
	jar cvf pack.jar *.java *.class ; \
	ls -l pack.jar ;


## ###################################################################
## [4] testCrypter : a test suite
## ###################################################################

.PHONEY: testsuite

TESTSUITEDIR=./testCrypter
TESTSUITE=./testCrypter.zip

testsuite:
	@if [ ! -d $(TESTSUITEDIR) ]; then mkdir $(TESTSUITEDIR) ; fi;
	rm -rf $(TESTSUITEDIR)/*
	rm -f $(TESTSUITE) ;
	cp Makefile_testCrypter $(TESTSUITEDIR)/Makefile;
	cp test/check_random_message $(TESTSUITEDIR) ;
	cp Crypter.java $(TESTSUITEDIR) ;
	cp KeyGeneratorCS.java $(TESTSUITEDIR) ;
	chmod 0644 $(TESTSUITEDIR)/* ;
	zip $(TESTSUITE) $(TESTSUITEDIR)/* ;


## ###################################################################
## [5] Publish
## ###################################################################

.PHONEY: publish

WWW_BASE=$(HOME)/Domain/Upload
WWW_TESTSUITEDIR=$(WWW_BASE)/sub


publish:
	cd Public; make clean all publish; 
	cd .millstone; make clean all publish;
	cd toy; make clean all publish; 
	make testsuite ;
	cp $(TESTSUITE) $(WWW_TESTSUITEDIR) ;
	echo "publish: done." 
	echo "NOTES:"; 
	echo " + remember to update crypter.html, applet-crypter.html, millstone_crypter.html, applet-millstone_crypter.html" ;\
	echo " + remember to update Validate (in ./Validate: call mkval.bash)";






