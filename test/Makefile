## #############################################################
## Makefile for test suite
## 
##   make check   : run a set of tests on dynamically allocated keys
##		    store errors in tmp.NNN directories
##   make vanilla : test the vanilla key
## 
## 
## Synopsis:
##  Contains Birthdays, Weddings ans similar
##  To be included in ~/diary
## -------------------------------------------------------------
## @TABLE OF CONTENTS:		       [TOCD: 10:57 20 Nov 2004]
##
##  [1] BACKUP THE LAST FAIL
##  [2] VANILLA PART
## #############################################################
## @FILE:    Makefile
## @PLACE:   Mars Workstation
## @FORMAT:  plain text
## @AUTHOR:  M. Oliver M'o'ller     <omoeller@verify-it.de>
## @BEGUN:   Sat Nov 20 10:56:01 2004
## @VERSION: Sat Nov 20 10:56:01 2004
## #############################################################


# N = 16 does not work properly on all file/encoding systems
N_TO_TEST= 7 8  
KEY_LENGTHS_TO_TEST=13 20 50 70 100 200 300 400 500 800 1000 2000
#MESSAGE_LENGTHS_TO_TEST=3 4 5 31 32 34
#MESSAGE_LENGTHS_TO_TEST=10 30  100 200 300 400 500 600 700 800 900 1000 2000
#MESSAGE_LENGTHS_TO_TEST=100 500 1000 2000
MESSAGE_LENGTHS_TO_TEST=1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 \
	200 500 1000 2000


.PHONEY: check save cleansave vanilla

check:
	@if [ ! -d tmp.000 ]; then mkdir tmp.000 ; fi ;
	@touch CryptoModule.java ;
	rm -f *.class ;
	@cp ../KeyGeneratorCS.java . ;
	javac KeyGeneratorCS.java ;
	@cp ../Crypter.java . ;
	@for k in $(KEY_LENGTHS_TO_TEST); do \
	    rm CryptoModule.java ; \
            java KeyGeneratorCS -n TestKey-$$k -o Test -f CryptoModule.java $$k; \
	    javac -classpath . Crypter.java; \
	    for m in $(MESSAGE_LENGTHS_TO_TEST); do \
		for n in $(N_TO_TEST); do \
		echo "** TESTING (MESSAGE_LENGTH, KEY_LENGTH, N) = ($$m, $$k, $$n)"; \
		if ! bash -x check_random_message $$m  $$n 2>/dev/null; then \
		   echo "ERROR WITH (MESSAGE_LENGTH, KEY_LENGTH, N) = ($$m, $$k, $$n)"; \
		   make save ; \
		   exit 1; \
		fi; \
		done;	\
	    done	\
	done;



## ###################################################################
## [1] BACKUP THE LAST FAIL
## ###################################################################

save:
	@export NUM=$$(($$(echo "1`ls | grep "tmp[.][0-9][0-9][0-9]" |sort | tail -c 4`")-999)) ; \
	export DIR="tmp.$$(printf "%03d" $${NUM})"; \
	echo "## SAVING IN DIR $${DIR} "; \
	mkdir $${DIR}; \
	cp *.java *.class message.* $${DIR} ; \
	echo "done" ;

cleansave:
	rm -rf tmp.[0-9][0-9][0-9] ;
	mkdir tmp.000 ;


## cutoff VANILLA ----------------------------------------------------

## ###################################################################
## [2] VANILLA PART
## ###################################################################

## check only the vanilla file

vanilla:
	@if [ ! -d tmp.000 ]; then mkdir tmp.000 ; fi ;
	rm -f *.class ;
	@cp ../Crypter.java . ;
	@cp ../CryptoModule.java . ;
	javac -classpath . Crypter.java ;
	    @for m in $(MESSAGE_LENGTHS_TO_TEST); do \
		for n in $(N_TO_TEST); do \
		echo "** TESTING Vanilla MESSAGE_LENGTH = $$m, N=$$n "; \
		if ! bash -x check_random_message $$m $$n  2>/dev/null; then \
		   echo "ERROR WITH Vanilla MESSAGE_LENGTH = $$m "; \
		   make save ; \
		   exit 1; \
		fi; \
		done;	\
	    done	


## ###################################################################
## EXHAUSTIVE CHECK (ALL MESSAGES)
## ###################################################################

EXHAUSTIVE_KEY_LENGTHS_TO_TEST=17 25 32
EXHAUSTIVE_N_TO_TEST=8

.PHONEY: exhaustive

exhaustive:
	@if [ ! -d tmp.000 ]; then mkdir tmp.000 ; fi ;
	@touch CryptoModule.java ;
	rm -f *.class ;
	@cp ../KeyGeneratorCS.java . ;
	javac KeyGeneratorCS.java ;
	@cp ../Crypter.java . 
	@rm -f found.txt ;
	@for k in $(EXHAUSTIVE_KEY_LENGTHS_TO_TEST); do \
	    NOTFOUND=true; \
	    while $${NOTFOUND}; do \
	      rm CryptoModule.java ; \
              java KeyGeneratorCS -n TestKey-$$k -o Test -f CryptoModule.java $$(($$k-1)) ; \
	      grep TestKey CryptoModule.java >> found.txt ; \
	      if grep -e "TestKey-$$k-$$k" CryptoModule.java; then \
		 NOTFOUND=false; \
	      fi; \
	    done; \
	    javac -classpath . Crypter.java; \
		for n in $(EXHAUSTIVE_N_TO_TEST); do \
		echo "** EXHAUSTIVE TESTING (MESSAGE_LENGTH, KEY_LENGTH, N_BITS) = ($$(($$k-1)), $$k, $$n)"; \
		if ! bash -x check_all_messages $$(($$k-1))  $$n 2>/dev/null ; then \
		   echo "ERROR WITH (MESSAGE_LENGTH, KEY_LENGTH, N) = ($$k, $$k, $$n)"; \
		   make save ; \
		   exit 1; \
		fi; \
	    done	\
	done;




