## #############################################################
## 
## Special makes -----------------------------------------------
##
## millstone: Generate the Millstone Version of the Crypter.
## 
##  
## 
## -------------------------------------------------------------
## 
## Synopsis:
##  Contains Birthdays, Weddings ans similar
##  To be included in ~/diary
## #############################################################
## @FILE:    Makefile
## @PLACE:   Gaia Home
## @FORMAT:  plain text
## @AUTHOR:  M. Oliver M'o'ller     <oliver.moeller@verified.de>
## @BEGUN:   Mon Feb  3 23:10:12 2003
## @VERSION: Mon Feb  3 23:10:12 2003
## #############################################################
## 


all:
	javac *.java


clean:
	rm *.class



## ###################################################################
## Millstone
## ###################################################################

.PHONEY:  millstone

millstone:
	echo "// Generated from Crypter.java on $(date)" |cat - Crypter.java \
	     |sed "s/Crypter/MillstoneCrypter/g" \
	     |sed "s/CryptoModule/MillstoneCryptoModule/g" \
	     > ./.millstone/MillstoneCrypter.java; \
	pushd ./.millstone;	\
	javac MillstoneCrypter.java; \
	popd;	\
	echo "makeMillstone: done.";

