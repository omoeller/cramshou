## #############################################################
## MAKE TARGETS
## 
## all      : create and compile your key pair (UserCryptoModule.java)
##
## check    : test your key pair
##
## jar:	    : pack the relevant java/class files into pack.jar 
## 
## testsuite: generate + pack files in the testCrypter directory
## 
## remove   : remove the UserCryptoModule.java
##
## clean    : clean the generated/compiled files (but leave UserCryptoModule.java)
##
## -------------------------------------------------------------
## 
## Synopsis:
##  Cramer Shoup Crypto System
## #############################################################
## @TABLE OF CONTENTS:		       [TOCD: 11:10 01 Nov 2015]
##
##  [1] Java Compile
##  [2] Test
##  [3] Jar
##  [4] testCrypter : a test suite
##  [5] Publish
## ##########################################################
## @FILE:    Makefile
## @PLACE:   *unkown*
## @FORMAT:  plain text
## @AUTHOR:  M. Oliver M'o'ller     <oliver.moeller@verified.de>
## @BEGUN:   Mon Feb  3 23:10:12 2003
## @VERSION: Sun Nov  1 11:01:43 2015
## #############################################################

## -- Adjust this to your personal settings --------------------------------

USER_NAME   = Miss Y
USER_EMAIL  = <X@nowhere.org>
USER_KEYLEN = 2048

## -------------------------------------
TEST_BITS = 8
## -------------------------------------------------------------------------

JAVA_FILES=Crypter.java CryptoModule.java

#J_FLAGS=-Xlint:unchecked -deprecation
J_FLAGS=-deprecation

OBJECT_FILES=$(patsubst %.java,%.class,$(JAVA_FILES))

all:	
	$(MAKE) UserCryptoModule.java
	cp UserCryptoModule.java CryptoModule.java
	$(MAKE) $(OBJECT_FILES)

clean:
	rm -f *.class CryptoModule.java

remove: clean
	rm -f UserCryptoModule.java

UserCryptoModule.java:
	$(MAKE) KeyGeneratorCS.class
	java KeyGeneratorCS -f $(@) -n "$(USER_NAME)" -o "$(USER_NAME) $(USER_EMAIL)" $(USER_KEYLEN)

## ###################################################################
## [1] Java Compile
## ###################################################################

%.class:	%.java
	javac $(J_FLAGS) $< ;


## ###################################################################
## [2] Test
## ###################################################################

.PHONEY:  check test_ordinary test_binary 

check:	 
	@echo "----------------------------------------------------------------------"
	@echo " This will "
	@echo "  (a) ensure your UserKey is build"
	@echo "  (b) compile the code (with javac)"
	@echo "  (c) try to encrypt-decrypt with it binary and ASCII input"
	@echo "----------------------------------------------------------------------"
	$(MAKE) all
	$(MAKE) test_ordinary test_binary 
	@echo " For more exhautive checks do: "
	@echo "     make -C ../test usertest"

test_ordinary: $(OBJECT_FILES) CryptoModule.class
	@echo "## -- testing encryption/decryption of ordinary input --------"
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
	@echo "## -- testing encryption/decryption of binary input --------"
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


info:
	@echo "For exhaustive checks, do "
	@echo "   make -C ../test check"


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

TESTSUITEDIR=./../testCrypter
TESTSUITE=./../testCrypter.zip

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
	$(MAKE) -C Public clean all publish

publish-testsuite:
	make testsuite ;
	cp $(TESTSUITE) $(WWW_TESTSUITEDIR) ;

## -------------------------------------------------------------------------





