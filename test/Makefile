
N_TO_TEST=7
KEY_LENGTHS_TO_TEST=32
#MESSAGE_LENGTHS_TO_TEST=3 4 5 31 32 34
MESSAGE_LENGTHS_TO_TEST=10 100 200 300 500

.PHONEY: check save cleansave

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
## BACKUP THE LAST FAIL
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


