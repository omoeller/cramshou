#!/usr/local/bin/bash
## #############################################################
## Creation of a validator program
##
## Relies on:
##               template    val.bash
##               existence of
##                           make-val-download
##                           make-val-remove-mime
##               in the emacs library my-web-access
##
##
## -- what currently works: ------------------------------------------------
# (defconst *wait-www-delay-seconds* 2)
#
# (defun my-wait-for-download ()
#   "Check for completion, if not, then queue yourself again."
#   (let ((buf (current-buffer))
# 	(res nil))
#     (set-buffer "*Messages*")
#     (goto-char (point-max))
#     (previous-line 5)
#     (if (search-forward-regexp "\\(^Drawing... done$\\|^Download.* complete[.]$\\)" 
# 			       (point-max) t)
# 	(setq res t))
#     ;; ---------------------------------------------------------------------
#     (set-buffer buf)
#     res))

    
# (defun my-w3-download-url-to-file (url file)
#   "Wrapper around some w3 functionality, which changed..."
#   (w3-download-url url file) ;; worked with older version...
#   (sleep-for 1)
#   (while (not (my-wait-for-download))
#     (sleep-for *wait-www-delay-seconds*))
#   (message "OK....\n\n\n\n\n")
#   )

  
# (defun make-val-download ()
#   (interactive)
#   (load-library "w3")
#   (my-w3-download-url-to-file "http://www.verify-it.de/applets/CryptoModule.java" "~/tmp/CryptoModule.java" )
#   (my-w3-download-url-to-file "http://www.verify-it.de/applets/CryptoModule.class" "~/tmp/CryptoModule.class" )
#   (my-w3-download-url-to-file "http://www.verify-it.de/applets/Crypter.java" "~/tmp/Crypter.java" )
#   (my-w3-download-url-to-file "http://www.verify-it.de/applets/Crypter.class" "~/tmp/Crypter.class" )
#   (my-w3-download-url-to-file "http://www.verify-it.de/sub/.millstone/MillstoneCrypter.java" "~/tmp/MillstoneCrypter.java" )
#   (my-w3-download-url-to-file "http://www.verify-it.de/sub/.millstone/MillstoneCrypter.class" "~/tmp/MillstoneCrypter.class" )
#   (my-w3-download-url-to-file "http://www.verify-it.de/sub/.millstone/MillstoneCryptoModule.java" "~/tmp/MillstoneCryptoModule.java" )
#   (my-w3-download-url-to-file "http://www.verify-it.de/sub/.millstone/MillstoneCryptoModule.class" "~/tmp/MillstoneCryptoModule.class" )
#   (my-w3-download-url-to-file "http://www.verify-it.de/applets/KeyGeneratorCS.java" "~/tmp/KeyGeneratorCS.java" )
#   )

# (defun my-remove-mime-start-of-file (file)
#   (find-file file)
#   (beginning-of-buffer)
#   (search-forward-regexp "^$")
#   (forward-char 1)
#   (delete-region (point-min) (point-marker))
#   (basic-save-buffer)
# ;;  (kill-buffer (current-buffer))
#   )

# (defun make-val-remove-mime ()
#   (interactive)
#   (my-remove-mime-start-of-file "~/tmp/CryptoModule.java" )
#   (my-remove-mime-start-of-file "~/tmp/CryptoModule.class" )
#   (my-remove-mime-start-of-file "~/tmp/Crypter.java" )
#   (my-remove-mime-start-of-file "~/tmp/Crypter.class" )
#   (my-remove-mime-start-of-file "~/tmp/MillstoneCrypter.java" )
#   (my-remove-mime-start-of-file "~/tmp/MillstoneCrypter.class" )
#   (my-remove-mime-start-of-file "~/tmp/MillstoneCryptoModule.java" )
#   (my-remove-mime-start-of-file "~/tmp/MillstoneCryptoModule.class" )
#   (my-remove-mime-start-of-file "~/tmp/KeyGeneratorCS.java" )
#   )
## -----------------------------------------------------------------------
##
## Synopsis:
##  Check consistency
## #############################################################
## @FILE:    mkval.bash
## @PLACE:   Linux Homestation
## @FORMAT:  Bash Script
## @AUTHOR:  M. Oliver M"oller     <omoeller@brics.dk>
## @BEGUN:   Sun Jun 25 18:03:44 2000
## @VERSION: Thu Jul 11 22:34:38 2002
## #############################################################
IN_FILE=val.bash
EXEC_FILE=val
## -- updating local files --------------------------------------
cp ../Public/CryptoModule.java Vanilla-2014.public.java
cp ../Public/CryptoModule.class Vanilla-2014.public.class
cp ../Public/Crypter.java Crypter.java
cp ../Public/Crypter.class Crypter.class
cp ../Public/CryptoModule.java CryptoModule.java
cp ../Public/CryptoModule.class CryptoModule.class
cp ../Public/KeyGeneratorCS.java KeyGeneratorCS.java
cp ../Public/KeyGeneratorCS.class KeyGeneratorCS.class
cp ../.millstone/MillstoneCryptoModule.java Millstone-5020.public.java
cp ../.millstone/MillstoneCryptoModule.class Millstone-5020.public.class
cp ../.millstone/MillstoneCrypter.java  MillstoneCrypter.java
cp ../.millstone/MillstoneCrypter.class MillstoneCrypter.class
## -- creating validation script --------------------------------
ARCHIV_FILES="*.class *.java"
echo "# !/usr/local/bin/bash" > $EXEC_FILE
echo -n "LINE=" >> $EXEC_FILE
echo $(($(cat $IN_FILE | wc -l)+1)) >> $EXEC_FILE
tail +3 $IN_FILE >> $EXEC_FILE
gtar cO $ARCHIV_FILES |gzip -c >> $EXEC_FILE
echo "Created: $EXEC_FILE"
chmod 0700 $EXEC_FILE
ls -l $EXEC_FILE
echo "done."