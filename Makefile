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
## -------------------------------------------------------------
## 
## Synopsis:
##  Contains Birthdays, Weddings ans similar
##  To be included in ~/diary
## #############################################################
## @TABLE OF CONTENTS:		       [TOCD: 00:30 04 Feb 2003]
##
##  [1] Java Compile
##  [2] Test
##  [3] Jar
##  [4] Millstone
##  [5] Public
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
	javac $< ;


## ###################################################################
## [2] Test
## ###################################################################

.PHONEY:  test

test:	 test_binary test_ordinary

test_binary: $(OBJECT_FILES)
	@if [ ! -d tmp ]; then mkdir tmp; fi ;
	cp binary.in ./tmp/test.in ; \
	date >> ./tmp/test.in ; \
	java Crypter e -$(TEST_BITS) ./tmp/test.in ./tmp/test.out ; \
	java Crypter d -$(TEST_BITS) ./tmp/test.out ./tmp/test.decrypted ; \
	if cmp -c ./tmp/test.in ./tmp/test.decrypted; then \
		echo "** Test successful." ; \
	else \
		echo "** Test FAILED!" ; \
		diff ./tmp/test.in ./tmp/test.decrypted ; \
		exit 7; \
	fi; \

test_ordinary: $(OBJECT_FILES)
	@if [ ! -d tmp ]; then mkdir tmp; fi ;
	cp ordinary.in ./tmp/ordinary.in ; \
	date >> ./tmp/test.in ; \
	java Crypter e -$(TEST_BITS) ./tmp/test.in ./tmp/test.out ; \
	java Crypter d -$(TEST_BITS) ./tmp/test.out ./tmp/test.decrypted ; \
	if cmp -c ./tmp/test.in ./tmp/test.decrypted; then \
		echo "** Test successful." ; \
	else \
		echo "** Test FAILED!" ; \
		diff ./tmp/test.in ./tmp/test.decrypted ; \
		exit 7; \
	fi; \


## ###################################################################
## [3] Jar
## ###################################################################

.PHONEY:	jar

jar:	$(OBJECT_FILES)
	\rm pack.jar ; \
	jar cvf pack.jar *.java *.class ; \
	ls -l pack.jar ;




## ###################################################################
## [5] Publish
## ###################################################################

WWW_BASE=$(HOME)/Domain/Upload

.PHONEY: publish

publish:
	cd Public; make clean all publish; cd ..; \
	cd .millstone; make clean all publish; cd .. ; \
	echo "publish: done." ;\
	echo "NOTES:"; \
	echo " + remember to update crypter.html, applet-crypter.html, millstone_crypter.html, applet-millstone_crypter.html" ;\
	echo " + remember to update Validate (mkval.bash)";





