## #############################################################
## 
## 
## Synopsis:
##  Crypter
## #############################################################
## @TABLE OF CONTENTS:		       [TOCD: 00:32 04 Feb 2003]
##
##  [1] Publish
## ##########################################################
## @FILE:    Makefile
## @PLACE:   Linux Homestation
## @FORMAT:  plain text
## @AUTHOR:  M. Oliver M'o'ller     <oliver.moeller@verified.de>
## @BEGUN:   Tue Feb  4 00:18:06 2003
## @VERSION: Wed Jul  7 07:57:14 2004
## #############################################################
## 

all:	\
	CryptoModule.java \
	Crypter.java \
	KeyGeneratorCS.java \
	Crypter.class \
	CryptoModule.class \
	KeyGeneratorCS.class 




clean:
	rm *.class \
		Crypter.java KeyGeneratorCS.java


## -- Copy all java - except CrypoModule.java ! ----------------------------


Crypter.java:	../Crypter.java
	cp ../Crypter.java Crypter.java

KeyGeneratorCS.java:	../KeyGeneratorCS.java
	cp ../KeyGeneratorCS.java KeyGeneratorCS.java


%.class:	%.java
	javac $< ;


## ###################################################################
## [1] Publish
## ###################################################################

WWW_BASE=$(HOME)/Domain/Upload

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

