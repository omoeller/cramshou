## #############################################################
## 
## 
## Synopsis:
##  Crypter
## #############################################################
## @TABLE OF CONTENTS:		       [TOCD: 22:24 06 Feb 2003]
##
##  [1] Test
##  [2] Publish
## ##########################################################
## @FILE:    Makefile
## @PLACE:   Gaia Home
## @FORMAT:  millstone/Makefile
## @AUTHOR:  M. Oliver M'o'ller     <oliver.moeller@verified.de>
## @BEGUN:   Tue Feb  4 00:36:03 2003
## @VERSION: Wed Feb  5 00:54:35 2003
## #############################################################
## 

OBJECT_FILES= \
	ToyCrypter.java \
	ToyCrypter.class \
	ToyCryptoModule.class 

TEST_BITS=8

all:	$(OBJECT_FILES)


clean:
	rm -f *.class ToyCrypter.java test.*

## -- Copy some java - except CrypoModule.java ! ----------------------------


ToyCrypter.java:	../src/Crypter.java
	echo "// Generated from Crypter.java on: $$(date)" \
		| cat - ../src/Crypter.java \
		|sed 's/Crypter/ToyCrypter/g' \
	        |sed 's/CryptoModule/ToyCryptoModule/g' \
	     	> ToyCrypter.java; \

ToyCryptoModule.class:	ToyCrypter.class

%.class:	%.java
	javac $< ;

## ###################################################################
## [1] Test
## ###################################################################

.PHONEY:  test

test:	$(OBJECT_FILES)
	cp ../src/binary.in ./test.in ; \
	date >> ./test.in ; \
	java ToyCrypter e -$(TEST_BITS) ./test.in ./test.out ; \
	java ToyCrypter d -$(TEST_BITS) ./test.out ./test.decrypted ; \
	java ToyCrypter u -$(TEST_BITS) ./test.out ./test.untruncated ; 
	@if cmp -c ./test.in ./test.decrypted; then \
		echo "** Test successful." ; \
	else \
		echo "** Test FAILED!" ; \
		diff ./test.in ./test.decrypted ; \
		exit 7; \
	fi; 


## ###################################################################
## [2] Publish
## ###################################################################

WWW_BASE=$(HOME)/Domain/Upload

.PHONEY:	publish

publish:
	make all; \
	cp Base64Handler.class $(WWW_BASE)/sub/toycrypt/ ; \
	cp CONST.class $(WWW_BASE)/sub/toycrypt/ ; \
	cp HashFunction.class $(WWW_BASE)/sub/toycrypt/ ; \
	cp Listener.class $(WWW_BASE)/sub/toycrypt/ ; \
	cp ToyCrypter.class $(WWW_BASE)/sub/toycrypt/ ; \
	cp ToyCryptoModule.class $(WWW_BASE)/sub/toycrypt/ ; \
	cp PublicKey.class $(WWW_BASE)/sub/toycrypt/ ; \
	cp SecretKey.class $(WWW_BASE)/sub/toycrypt/ ; \
	cp ToyCrypter.java $(WWW_BASE)/sub/toycrypt/ ; \
	cp ToyCryptoModule.java $(WWW_BASE)/sub/toycrypt/ ; \
	chmod 0644 $(WWW_BASE)/sub/toycrypt/*.class ; \
	chmod 0644 $(WWW_BASE)/sub/toycrypt/ToyCrypt*.java ; \
