# !/usr/local/bin/bash
##LINE=?? ## DO NOT MODIFY THIS LINE.
## #############################################################
## Validation of some files
## 
## Synopsis:
##  Input for mkval.bash
##  [this generates standalone executable val]
## #############################################################
## @FILE:    val.bash
## @PLACE:   Linux Homestation
## @FORMAT:  Bash Script
## @AUTHOR:  M. Oliver M"oller     <omoeller@brics.dk>
## @BEGUN:   Sun Jun 25 18:16:01 2000
## @VERSION: Tue Oct  5 07:12:25 2004
## #############################################################
## -- files to validate ------------------
VAL[1]=Vanilla-2014.public.java
CMP[1]=~/Domain/Upload/applets/CryptoModule.java
VAL[2]=Vanilla-2014.public.class
CMP[2]=~/Domain/Upload/applets/CryptoModule.class
VAL[3]=Crypter.java
CMP[3]=~/Domain/Upload/applets/Crypter.java
VAL[4]=Crypter.class
CMP[4]=~/Domain/Upload/applets/Crypter.class
#---
VAL[5]=Millstone-5020.public.java
CMP[5]=~/Domain/Upload/sub/.millstone/MillstoneCryptoModule.java
VAL[6]=Millstone-5020.public.class
CMP[6]=~/Domain/Upload/sub/.millstone/MillstoneCryptoModule.class
VAL[7]=MillstoneCrypter.java
CMP[7]=~/Domain/Upload/sub/.millstone/MillstoneCrypter.java
VAL[8]=MillstoneCrypter.class
CMP[8]=~/Domain/Upload/sub/.millstone/MillstoneCrypter.class
# --------------------------------------------------
VAL[9]=Crypter.java
CMP[9]=~/tmp/Crypter.java
VAL[10]=Crypter.class
CMP[10]=~/tmp/Crypter.class
VAL[11]=Vanilla-2014.public.java
CMP[11]=~/tmp/CryptoModule.java
VAL[12]=Vanilla-2014.public.class
CMP[12]=~/tmp/CryptoModule.class
VAL[13]=MillstoneCrypter.java
CMP[13]=~/tmp/MillstoneCrypter.java
VAL[14]=MillstoneCrypter.class
CMP[14]=~/tmp/MillstoneCrypter.class
VAL[15]=Millstone-5020.public.java
CMP[15]=~/tmp/MillstoneCryptoModule.java
VAL[16]=Millstone-5020.public.class
CMP[16]=~/tmp/MillstoneCryptoModule.class
## ----------------------------------------------------
VAL[17]=KeyGeneratorCS.java
CMP[17]=~/tmp/KeyGeneratorCS.java 
VAL[18]=KeyGeneratorCS.class
CMP[18]=~/tmp/KeyGeneratorCS.class
VAL[19]=testCrypter.zip
CMP[19]=~/tmp/testCrypter.zip
N=19;
## --------------------
TEMP=./__compare_file.$$
## --
function get_file()
{
  tail +$LINE $0 | gtar xzfO - $1 > $TEMP;
}
ERRORS=0;
I=1;
## DOWNLOAD the files before checking -- prevent corruption through server
echo ""
echo ">>>> Download the files freshly from Domain with   (make-val-download)...."
echo ""
if emacs -batch --eval '(progn (load-file "~/.emacs")(load-library "om-web-access")(make-val-download) (save-buffers-kill-emacs))'; then
    echo "ERROR: should exit with code 7.";
    exit 1;
else
  if [ "$?" != "7" ]; then
    echo ">>>> Return status: $?"
    echo "ERROR: download not successuful.";
    exit 1;
  fi;
  echo ">>>> Return status: $?"
fi;
echo ""
echo ""
echo ">>>> fixing mime...."
emacs -batch --eval '(progn (load-file "~/.emacs")(load-library "om-web-access")(make-val-remove-mime) (save-buffers-kill-emacs))'
## --------------------
while [ $I -le $N ]; do
    echo "Validation $I:";
    echo " ${VAL[$I]}   vs.";
    echo " ${CMP[$I]}";
    get_file ${VAL[$I]};
#    if cmp   ${VAL[$I]} $TEMP; then
    if cmp   ${CMP[$I]} $TEMP; then
       echo "OK.";
       echo "";
    else
       ERRORS=$(($ERRORS+1));
    fi;
    I=$(($I+1));  
done;
rm $TEMP;
echo "** Validation done."
echo "** $ERRORS errors."
exit 1;
exit 0;
## -- Archive follows this line ---
