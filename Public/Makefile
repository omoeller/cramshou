## #############################################################
## 
## 
## Synopsis:
##  Cramer Shoup Crypto System
## #############################################################
## @TABLE OF CONTENTS:		       [TOCD: 11:01 01 Nov 2015]
##
##  [1] NORMAL MAKE TARGETS
##  [2] Publish
## ##########################################################
## @FILE:    Makefile
## @PLACE:   Linux Homestation
## @FORMAT:  plain text
## @AUTHOR:  M. Oliver M'o'ller     <oliver.moeller@verified.de>
## @BEGUN:   Tue Feb  4 00:18:06 2003
## @VERSION: Wed Jul  7 07:57:14 2004
## #############################################################

## -- Adjust this to your personal settings --------------------------------

WWW_BASE=$(HOME)/MyDomain/Upload

## -------------------------------------------------------------------------
J_FLAGS=-deprecation


## ###################################################################
## [1] NORMAL MAKE TARGETS
## ###################################################################

all:	\
	CryptoModule.java \
	Crypter.java \
	KeyGeneratorCS.java \
	Crypter.class \
	CryptoModule.class \
	KeyGeneratorCS.class 

clean:
	rm -f *.class \
		CrytoModule.java \
		Crypter.java KeyGeneratorCS.java

remove:	clean
	rm -f CryptoModule.java

## -- Copy all java --------------------------------------------------

## -- in CryptoModule.java, remove the secret key part with sed !
CryptoModule.java: ../src/UserCryptoModule.java
	cat $< | sed 's/\(byte[[][]] \(x1\|x2\|y1\|y2\|z\) =\).*/\1 { 0 }; /g' \
	   > $@ ;
	@echo "-- MANUALLY CONFIRM THAT THE SECRET KEY HAS BEEN REMOVED (should all be 0) --"
	grep -A 5 "byte.. x1 =" $@
	@echo "-- ----------------------------------------------------------------"

Crypter.java:	../src/Crypter.java
	@cp $< $@

KeyGeneratorCS.java:	../src/KeyGeneratorCS.java
	@cp $< $@

%.class:	%.java
	javac $(J_FLAGS) $< ;


## ###################################################################
## [2] Publish
## ###################################################################

.PHONEY:	publish

publish:	
	cp Base64Handler.class $(WWW_BASE)/applets/ ; \
	cp CONST.class $(WWW_BASE)/applets/ ; \
	cp Crypter.class $(WWW_BASE)/applets/ ; \
	cp CryptoModule.class $(WWW_BASE)/applets/ ; \
	cp HashFunction.class $(WWW_BASE)/applets/ ; \
	cp Listener.class $(WWW_BASE)/applets/ ; \
	cp PublicKey.class $(WWW_BASE)/applets/ ; \
	cp KeyGeneratorCS.class $(WWW_BASE)/applets/ ; \
	chmod 0755 $(WWW_BASE)/applets/*.class ; \
	cp Crypter.java $(WWW_BASE)/applets/ ; \
	cp CryptoModule.java $(WWW_BASE)/applets/ ; \
	cp KeyGeneratorCS.java $(WWW_BASE)/applets/ ; \
	chmod 0644 $(WWW_BASE)/applets/Crypt*.java ; 

