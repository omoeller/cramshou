;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; My-Web-Access
;;;
;;; Collection of functions that allow some internet browsing.
;;;
;;; Uses:
;;;   the w3 browser
;;;
;;; Synopsis:
;;;  Emacs macros for w3
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; @TABLE OF CONTENTS:			[TOCD: 16:39 18 May 2003]
;;;
;;;      [0.1] Auto-Load
;;;      [0.2] Variable Settings
;;;      [0.3] Hooks
;;;  [1] Functions
;;;      [1.1] ;; other w3 stuff
;;;          [1.1.1] AUX
;;;  [2] Snarfing Web Sites
;;;  [3] Fixes to W3 annoyances
;;; /////////////////////////////////////////////////////////
;;; @FILE:    my-web-access.el
;;; @PLACE:   Gaia Homestation
;;; @FORMAT:  Emacs lisp
;;; @AUTHOR:  M. Oliver M'o'ller     <omoeller@brics.dk>
;;; @BEGUN:   Mon Sep 18 17:18:17 2000
;;; @VERSION: Thu Sep 11 07:04:40 2003
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 

(require 'my-lisp-macros)
(require 'w3)

(defvar mailcap-mime-extensions nil "Dummy")

(defvar url-be-asynchronous t)

;;; //////////////////////////////////////////////
;;; [0.1] Auto-Load
;;; //////////////////////////////////////////////

(condition-case () (require 'w3-auto "w3-auto") (error nil))
	
;;; //////////////////////////////////////////////
;;; [0.2] Variable Settings
;;; //////////////////////////////////////////////

(defvar *web-fetch-first-comic* t
"Initially true, then set to nil.")


(defvar *my-w3-history-stack* nil)    


;;; //////////////////////////////////////////////
;;; [0.3] Hooks
;;; //////////////////////////////////////////////


(defun my-w3-init ()
  (interactive)

  (local-set-key " " #'my-w3-follow-link)
  (local-set-key "c" #'my-w3-history-backward))


(add-hook 'w3-mode-hook
	  #'my-w3-init)


;;; //////////////////////////////////////////////////////////////////////
;;; [1] Functions
;;; //////////////////////////////////////////////////////////////////////


(defun do-w3-die (url)
  "Try to fetch a site.

!!! CAUTION !!!
If the site does not exist, emacs dies (SIGKILL)."
  (interactive)
  (let ((displayed-lines-as-message 3) ;; set to nil, if undesired
	(error-pattern (format "\\(%s\\|%s\\|%s\\|%s\\)"
			       "^404[ \n\t\r]+Not[ \n\t\r]+Found"
			       "Error[ \n\t\r]+accessing[ \n\t\r]+"
			       "^\*Not[ \t\r\n]+Found\*"
			       "----[ \n\t\r]+error[ \n\t\r]+was:[ \n\t\r]+----[\n ]*unknown[ \n\t\r]+host[ \n\t\r]+")))
    (w3-fetch url)
    (my-wait-for-download)
    (sleep-for 2)
    (setq case-fold-search t)
    (goto-char (point-min))
  
    (if (search-forward-regexp error-pattern (point-max) t)
	(progn (message "*die with error*")
	       ;;(kill-ring-save (point-min) (point-max))(message (format "%s" (car kill-ring)))
	       (message "FAILURE")
	       (kill-emacs 1)) ;; emacs -batch dies on errors
	(progn (message "** OK Quit")
	       ;;(kill-ring-save (point-min) (point-max))(message (format "%s" (car kill-ring)))
	       (if displayed-lines-as-message 
		   (progn ;; -- show first lines (no error, if shorter)
		     (goto-char (point-min))
		     (forward-line displayed-lines-as-message) 
		     (end-of-line)
		     (if (> (point-marker) (point-min)) 
			 (progn (kill-ring-save (point-min) (point-marker))(message (format "%s" (car kill-ring)))))))
	       ;; -------------------------------------
	       (kill-emacs 7)))))


(defconst *wait-www-delay-seconds* 2)

(defun my-wait-for-download ()
  "Check for completion, if not, then queue yourself again."
  (let ((buf (current-buffer))
	(res nil))
    (set-buffer "*Messages*")
    (goto-char (point-max))
    (previous-line 3)
    (if (search-forward-regexp "\\(^Drawing... done$\\|^Download.* complete[.]\\|['] complete.$\\)" 
			       (point-max) t)
	(setq res t))
    ;; ---------------------------------------------------------------------
    (set-buffer buf)
    res))

(defun my-wait-for-completion ()
  "Check for completion, if not, then queue yourself again."
  (let ((buf (current-buffer))
	(res nil))
    (set-buffer "*Messages*")
    (goto-char (point-max))
    (previous-line 5)
    (if (search-forward-regexp "^Drawing... done" 
			       (point-max) t)
	(setq res t))
    ;; ---------------------------------------------------------------------
    (set-buffer buf)
    res))


(defun retrieve-dilbert-number ()
  (let ((result "FAIL")
	(pos nil)
	(tmp nil)
	(buf (current-buffer)))
    ;; ---------------------------------------------------------
    (set-buffer "*Messages*")
    (previous-line 5)
    (setq pos (point-marker))
    (goto-char (point-max))
    ;; ---------------------------------------------------------
    (while (null (search-backward-regexp "complete.$" pos t))
      (sleep-for 2)
      (goto-char (point-max)))
    (message "COMPLETED DOWNLOAD")
    ;; ---------------------------------------------------------
    (goto-char (point-max))
    (if (search-backward-regexp "images[/]dilbert\\([0-9]+\\)[.]gif" (point-min) t)
	(progn
	  (copy-region-as-kill (point-min) (point-max))
	  ;; --------------------------------------------
	  (set-buffer "*scratch*")
	  (goto-char (point-max))
	  (setq pos (point-marker))
	  (yank)
	  (message (format "** MARK REACHED my-web-access.el:Line 142" ))
	  (set-buffer "*scratch*")
	  (goto-char (point-max))
	  (search-backward-regexp "images[/]dilbert\\([0-9]+\\|FAIL\\)[.]gif" (point-min))
	  (message (format "** MARK REACHED my-web-access.el:Line 145" ))
	  (copy-region-as-kill (match-beginning 1)
			       (match-end 1))
	  (setq result (car kill-ring))
	  (delete-region pos (point-max))
	  (set-buffer "*Messages*")
	  (message (format "yy: %s" result))
	  ))
    ;; ---------------------------------------------------------
    (goto-char (point-max))
    (set-buffer buf)
    result))
    

(defun web-get-comic (site &rest pattern)
  "With optional second argument non-nil,
search for the dilbert number."
  (interactive)
  (setq case-fold-search t)
  (let ((buf nil)
	(bul nil)
	(result "NOT-RETRIEVED")
	(retrieve (and pattern (cdr pattern)))
	(pat (if (null pattern)
		 "\\([Tt]oday's\\|comic strip[^s]\\)"
		 (car pattern))))
    (setq w3-display-frames nil)
    (setq buf (current-buffer))
    (w3-fetch site)
    (while (not (my-wait-for-completion))
      (sleep-for *wait-www-delay-seconds*))
    ;; -- switch to the HTML buffer -------------------
    (setq bul (buffer-list))
    (while (progn
	     (set-buffer (car bul))
	     (message (format "** Switching to %s" (car bul)))
	     (setq bul (cdr bul))
	     (or
	      (equal (aref "*" 0) (aref (buffer-name) 0))
	      (equal (aref "*" 0) (aref (buffer-name) 1))))
      nil)
    ;; ------------------------------------------------
;;    (message "Hunting..")
    (message (format "Hunting...in %s" (buffer-name)))
    (goto-char (point-min))
    (beginning-of-buffer)
    (sleep-for 1)
    (search-forward-regexp pat)
    (backward-char 2)
      ;;  (if (not *web-fetch-first-comic*)
      ;;      (run-with-timer 3 99999999 'answer-with-a-y))
    (message "Now loading image...")
    (w3-follow-inlined-image)
    (if retrieve
	(setq result (retrieve-dilbert-number)))
    ;; (widget-button-press (point))
    ;;  (setq *web-fetch-first-comic* nil)
    ;;  (w3-follow-link)
    ;;  (widget-button-click (list 'down-mouse-2 (list (get-buffer-window (current-buffer)) 1586 (cons 260  463) 122472708)))
    
    ;;  (search-forward name)
    ;;  (search-forward-regexp "\\(today\\|comic strip\\)")
    ;;  (backward-char 2)
    ;;  (w3-follow-link)
    (kill-buffer (current-buffer))
    ;; ---------------------------------------------------------
    result
    ))

	;; dailies/SL03.01.29.gif
;;http://www.slagoon.com/dailies/SL03.01.29.gif

;;http://www.unitedmedia.com/comics/pearls/archive/images/pearls20012193730212.gif
(defun web-get-pearls ()
  (interactive)

   (w3-fetch (format "http://www.unitedmedia.com/comics/pearls/archive/images/%s"
		     (format-time-string "pearls2001219373%m%d.gif"))))
  
;;pearls20012193730212.gif

(defun web-get-sherman ()
  (interactive)

   (w3-fetch (format "http://cgibin.rcn.com/fillmore.dnai/dailies/%s"
		     (format-time-string "SL%y.%m.%d.gif"))))
;;  (w3-fetch "http://cgi.dnai.com/~fillmore/cgi-bin/sviewer.pl")
;;  (w3-fetch "http://cgibin.rcn.com/fillmore.dnai/cgi-bin/sviewer.pl")
;;  (while (not (my-wait-for-completion))
;;    (sleep-for *wait-www-delay-seconds*))
;;  (beginning-of-buffer)
;; (search-forward "[SL")
;;  (w3-follow-inlined-image)
;;  (kill-buffer (current-buffer)))


(defun web-get-otti ()
  (interactive)
  ;;http://www.mopo.de/images/2003/20030130/otto20030130.gif
  (w3-fetch (format-time-string "http://www.mopo.de/images/%Y/%Y%m%d/otto%Y%m%d.gif")))

(defun web-get-otti-old ()
  (interactive)
  (let ((bul nil))
    (w3-fetch "http://www.mopo.de/applications/fotoserien/index.php")
    (while (not (my-wait-for-completion))
      (sleep-for *wait-www-delay-seconds*))
    ;; -- switch to the HTML buffer -------------------
    (setq bul (buffer-list))
    (while (progn
	     (set-buffer (car bul))
	     (message (format "** Switching to %s" (car bul)))
	     (setq bul (cdr bul))
	     (or
	      (equal (aref "*" 0) (aref (buffer-name) 0))
	      (equal (aref "*" 0) (aref (buffer-name) 1))))
      nil)
    ;; --------------------------------------------------
    (end-of-buffer)
    (search-backward-regexp "[0-9]+[.][0-9]+[.][0-9]+")
    (forward-char 2)
    (w3-follow-inlined-image)
    ;;  (widget-button-press (point))
    (kill-buffer (current-buffer))))

(defun yesterday-time ()
  (let* ((ct (current-time))
	 (c0 (nth 0 ct))
	 (c1 (nth 1 ct))
	 ;; ----------------------------------------------------
	 (y1 (- c1 20846))
	 (y0 (- c0 1)))
    (if (< y1 0)
	(setq y1 (+ y1 65536))
	(setq y0 (- y0 1)))
    (list y0 y1)))
    ;;; 60*60*24 = 86400


;;
;;  (message (format-time-string "http://www.userfriendly.org/cartoons/archives/%y%b/uf005514.gif"))
;;  (w3-fetch (format-time-string "http://www.userfriendly.org/cartoons/archives/%y%b/uf005514.gif"))

;;  (kill-buffer "*scratch*")
;;  (trace-function 'widget-button-click "*scratch*")
(defun web-get-userfriendly ()
  (interactive)
    (web-get-comic 
     ;;(format-time-string "http://ars.userfriendly.org/cartoons/?id=%Y%m%d")
   "http://www.userfriendly.org"
   (format "\\(Latest Strip\\|%s\\|%s\\)"
	   (format-time-string "Strip for [a-zA-Z]+ %d, %Y")
	   (format-time-string "Strip for [a-zA-Z]+ %d, %Y" (yesterday-time)))))

  
(defun web-comic-load ()
  (interactive)
  
  (let ((hidden-string ""))
  ;; -----------------------------------------------------------------------
  (setq hidden-string
	(web-get-comic "http://www.unitedmedia.com/comics/dilbert/index.html" 
		       "\\([Tt]oday's\\|comic strip[^s]\\)"
		       'retrieve-dilbert-number))

  (message (format "** xx %s" hidden-string))
  ;; -----------------------------------------------------------
  (web-get-sherman)
  (web-get-otti)
  (web-get-userfriendly)
  ;; -----------------------------------------------------------
  (if (string= "0" (format-time-string "%w"))
      (web-comic-load-classic)
      (web-comic-load-with-hidden-string hidden-string)
      )
  ;; -----------------------------------------------------------
  (while (null (query-approval-default-no "Continue and quit")))
))


;;(defun foo () (interactive) (message (format-time-string ">>%w<<")))

(defun web-comic-load-with-hidden-string (hidden-string)
  (w3-fetch (format "http://www.unitedmedia.com/comics/pearls/archive/images/pearls%s.gif" hidden-string))
  ;;(web-get-comic "http://www.unitedmedia.com/comics/pearls/index.html"  "\\[pearls")
  ;;(web-get-pearls)
  ;; -----------------------------------------------------------
  ;;http://www.unitedmedia.com/comics/luann/archive/images/luann2003073162214.gif
  ;;http://www.unitedmedia.com/comics/luann/archive/images/luann23658140030217.gif
  ;;http://www.unitedmedia.com/comics/pearls/archive/images/pearls23658140030217.gif
  ;;http://www.unitedmedia.com/comics/dilbert/archive/images/dilbert23658140030217.gif
  (w3-fetch (format "http://www.unitedmedia.com/comics/luann/archive/images/luann%s.gif" hidden-string)) 

  ;;  (web-get-comic "http://www.unitedmedia.com/comics/luann/index.html" "\\[luann[0-9][0-9]")
  ;; --------------------------------------------------
  (w3-fetch (format "http://www.unitedmedia.com/comics/reality/archive/images/reality%s.gif" hidden-string))
  ;;(web-get-comic "http://www.unitedmedia.com/comics/reality/index.html" "\\[reality")
  ;; --------------------------------------------------
  (w3-fetch (format "http://www.unitedmedia.com/comics/hedge/archive/images/%s.gif" hidden-string))
;  (web-get-comic "http://www.unitedmedia.com/comics/hedge/index.html" "\\[hedge")
  ;; -----------------------------------------------------------
  (w3-fetch (format "http://www.unitedmedia.com/comics/franknernest/archive/images/franknernest%s.gif" hidden-string))
;  (web-get-comic "http://www.unitedmedia.com/comics/franknernest/index.html" "\\[franknernest")
  ;; -----------------------------------------------------------------------
;;  (web-get-comic "http://www.unitedmedia.com/comics/meg/index.html" "\\[meg")

  ;; -----------------------------------------------------------------------
)

(defun web-comic-load-classic ()
  (interactive)
  ;; -----------------------------------------------------------------------
  (web-get-comic "http://www.unitedmedia.com/comics/dilbert/" "Today.s Dilbert Comic")
  ;; -----------------------------------------------------------------------
;  (web-get-comic "http://www.unitedmedia.com/comics/pearls/index.html" ); "\\[pearls")
  ;; --------------------------------------------------
  (web-get-comic "http://www.unitedmedia.com/comics/luann/index.html"); "\\[luann[0-9][0-9]")
  ;; --------------------------------------------------
  (web-get-comic "http://www.unitedmedia.com/comics/reality/index.html"); "\\[reality")
  ;; --------------------------------------------------
  (web-get-comic "http://www.unitedmedia.com/comics/hedge/index.html"); "\\[hedge")
  ;; -----------------------------------------------------------
  (web-get-comic "http://www.unitedmedia.com/comics/franknernest/index.html"); "\\[franknernest")
  ;; -----------------------------------------------------------------------
  (web-get-comic "http://www.unitedmedia.com/comics/meg/index.html" ); "\\[meg")
  ;; -----------------------------------------------------------------------
;  (web-get-comic "http://www.unitedmedia.com/comics/dilbert/" "[[]today.s")
)


(defun web-cr-comic-load ()
  (interactive)
  (let ((bul nil)
	(pos nil))
    (w3-fetch "http://www.krisken.hpg.ig.com.br/cartoon.htm")
    (while (not (my-wait-for-completion))
      (sleep-for *wait-www-delay-seconds*))
    ;; ------------------------------------------------
    (setq bul (buffer-list))
    (while (progn
	     (set-buffer (car bul))
	     (message (format "** Switching to %s" (car bul)))
	     (setq bul (cdr bul))
	     (or
	      (equal (aref "*" 0) (aref (buffer-name) 0))
	      (equal (aref "*" 0) (aref (buffer-name) 1))))
      nil)
    ;; ------------------------------------------
    (beginning-of-buffer)
    (search-forward "Colored Cartoons")
    (search-forward "Name")
    (setq pos (point-marker))
    (while (progn
	     (w3-fetch "http://www.krisken.hpg.ig.com.br/cartoon.htm")
	     (goto-char pos)
	     (search-forward "[[" (point-max) t))
      (forward-char 1)
;;;    (w3-follow-inlined-image)
      (setq pos (point-marker))
      (widget-button-press (point))
;;      (w3-follow-inlined-image)
      )
    (if (query-approval-default-no "Done")
	nil
	nil)))


(defun web-cb-comic-load ()
  (interactive)
  (let ((pos nil))
    (w3-fetch "http://www.krisken.hpg.ig.com.br/cartoon.htm")
;    (search-forward "Black")
;    (end-of-line)
    (goto-char (point-min))
    (search-forward-regexp "Black .* White Cartoons")
    (setq pos (point-marker))
    (while (progn
	     (w3-fetch "http://www.krisken.hpg.ig.com.br/cartoon.htm")
	     (goto-char pos)
	     (search-forward "[[" (point-max) t))
      (forward-char 1)
;;;    (w3-follow-inlined-image)
      (setq pos (point-marker))
      (widget-button-press (point)))
    (if (query-approval-default-no "Done")
	nil
	nil)))


(defun my-w3-download-url-to-file (url file)
  "Wrapper around some w3 functionality, which changed..."
  (w3-download-url url file) ;; worked with older version...
  (sleep-for 1)
  (while (not (my-wait-for-download))
    (sleep-for *wait-www-delay-seconds*))
  (message "OK....\n\n\n\n\n\n")
  )


(defun my-list-processes-to-messages ()
  "Creates the list of emacs processes and transfers each line of it
to messages"
  (list-processes)
  (let ((buf (current-buffer))
	(tmp nil))
    (set-buffer "*Process List*")
    (goto-char (point-min))
    (while (< (point-marker) (point-max))
      (copy-region-as-kill (point-marker) (point-end-of-this-line))
      (message (car kill-ring))
      (end-of-line)
      (if (< (point-marker) (point-max))
	  (forward-char 1)))
    ;; ---------------------------------------------------------
    (set-buffer buf)))
    
      

  
(defun make-val-download ()
  (interactive)
  (load-library "w3")
  (my-w3-download-url-to-file "http://www.verify-it.de/applets/CryptoModule.java" "~/tmp/CryptoModule.java" )
  (my-w3-download-url-to-file "http://www.verify-it.de/applets/CryptoModule.class" "~/tmp/CryptoModule.class" )
  (my-w3-download-url-to-file "http://www.verify-it.de/applets/Crypter.java" "~/tmp/Crypter.java" )
  (my-w3-download-url-to-file "http://www.verify-it.de/applets/Crypter.class" "~/tmp/Crypter.class" )
  (my-w3-download-url-to-file "http://www.verify-it.de/sub/.millstone/MillstoneCrypter.java" "~/tmp/MillstoneCrypter.java" )
  (my-w3-download-url-to-file "http://www.verify-it.de/sub/.millstone/MillstoneCrypter.class" "~/tmp/MillstoneCrypter.class" )
  (my-w3-download-url-to-file "http://www.verify-it.de/sub/.millstone/MillstoneCryptoModule.java" "~/tmp/MillstoneCryptoModule.java" )
  (my-w3-download-url-to-file "http://www.verify-it.de/sub/.millstone/MillstoneCryptoModule.class" "~/tmp/MillstoneCryptoModule.class" )
  (my-w3-download-url-to-file "http://www.verify-it.de/applets/KeyGeneratorCS.java" "~/tmp/KeyGeneratorCS.java" )
  (my-w3-download-url-to-file "http://www.verify-it.de/applets/KeyGeneratorCS.class" "~/tmp/KeyGeneratorCS.class" )
  ;; -----------------------------------------------------------
  (sleep-for 3)
  (message "-- listing still active processes:")
  (my-list-processes-to-messages)
  (message "-- done.")
  ;; -----------------------------------------------------------
  (message "** Emacs make-valdownload: complete. exiting with code 7.")
  (kill-emacs 7)
  )

(defun my-remove-mime-start-of-file (file)
  (find-file file)
  (beginning-of-buffer)
  (search-forward-regexp "^$")
  (forward-char 1)
  (delete-region (point-min) (point-marker))
  (basic-save-buffer)
  (kill-buffer (current-buffer))
  )

(defun make-val-remove-mime ()
  (interactive)
  (my-remove-mime-start-of-file "~/tmp/CryptoModule.java" )
  (my-remove-mime-start-of-file "~/tmp/CryptoModule.class" )
  (my-remove-mime-start-of-file "~/tmp/Crypter.java" )
  (my-remove-mime-start-of-file "~/tmp/Crypter.class" )
  (my-remove-mime-start-of-file "~/tmp/MillstoneCrypter.java" )
  (my-remove-mime-start-of-file "~/tmp/MillstoneCrypter.class" )
  (my-remove-mime-start-of-file "~/tmp/MillstoneCryptoModule.java" )
  (my-remove-mime-start-of-file "~/tmp/MillstoneCryptoModule.class" )
  (my-remove-mime-start-of-file "~/tmp/KeyGeneratorCS.java" )
  (my-remove-mime-start-of-file "~/tmp/KeyGeneratorCS.class" )
  )



(if nil 
    (progn
	  (find-file "~/tmp/.dummy.txt")
	  (setq default-directory "~/tmp")
  (my-loop-for-i-from-to-do 1 6
			    ;;			    (w3-fetch "file:///users/omoeller/public_html/sub/val-load.html")
			    
			    
			    (goto-char (point-min))
			    (search-forward (format "%d" i))
			    (backward-char 1)
			    ;;			    (w3-download-url-at-point )))
			    (widget-button-press (point))
)))

;;; //////////////////////////////////////////////
;;; [1.1] ;; other w3 stuff
;;; //////////////////////////////////////////////

(defun leo-translate-word ()
    (interactive)
    (let ((word (get-this-word-mute)))
      (switch-to-buffer-other-window "LEO English/German Dictionary")
      (w3-fetch (format "http://dict.leo.org/?search=%s" word))))

(defun dig-csb-for-this-line ()
  (interactive)
  (let ((now (point-marker))
	(pos nil))
    (beginning-of-line)
    (setq pos (point-marker))
    (end-of-line)
    (kill-ring-save pos (point-marker))
    (goto-char now)
    (w3-fetch (format "www.brics.dk/~omoeller/sub/dig-csb.html?query=%s"
		      (car kill-ring)))
    (goto-char (point-min))
    (search-forward "Query:")
    (forward-char 3)
    (yank)
    (search-forward "[Sear")
    (widget-button-press (point))))    

;;; //////////////////////////////////////////////
;;; [1.1.1] AUX Functions
;;; //////////////////////////////////////////////

(defun answer-with-a-y () "Wait for user input request, then insert a 'y'."
  (interactive)
;  (message "Answer started.")
;  (insert "y")
;  (self-insert-and-exit)
;  (message "Answer finished.")
  )
;  (let ((waiting t))
;    (while waiting
;      (progn
;	;;(message (format "User input: %b." (waiting-for-user-input-p)))
;	(if (waiting-for-user-input-p)
;	    (progn
;	      (insert "y")
;	      (setq waiting nil)
;	      (message "Answer finished.")))))))


;;; //////////////////////////////////////////////////////////////////
;;; [2] Snarfing Web Sites
;;; //////////////////////////////////////////////////////////////////

;(defvar *fetch-and-save-as-dir* "~/tmp")
(defvar *fetch-and-save-as-dir* "~/Domain/Upload/pic/kohltour03")

(defun my-fetch-and-save-url (url)
  "Fetch and save some document; 
only works on flat directory structures.

Uses the variable *fetch-and-save-as-dir* to determine where to save."
  (let* ((filename (stripoff-path-from-filename url))
	 (file (scon *fetch-and-save-as-dir*
				      "/"
				      filename)))
    (my-w3-download-url-to-file url file)
    (my-remove-mime-start-of-file file)))

    



;;; //////////////////////////////////////////////////////////////////
;;; [3] Fixes to W3 annoyances
;;; //////////////////////////////////////////////////////////////////




(defun my-w3-follow-link ()
  (interactive)
  (let ((current-url (url-view-url 'no-show)))
    (setq *my-w3-history-stack* (cons current-url *my-w3-history-stack*))
    (w3-follow-link)))


(defun my-w3-history-backward ()
  (interactive)
  (if *my-w3-history-stack*
      (progn
	(w3-fetch (car *my-w3-history-stack*))
	(setq *my-w3-history-stack* (cdr *my-w3-history-stack*)))
      (message "** No history. Sorry.")))


;(setq w3-force-conversion-alist
;      `(("*.gcov" . ctext-unix)))




(setq mailcap-mime-extensions
      (cons
       '(".gcov" . "text/text")
       mailcap-mime-extensions))

;;; ------------------------------------------------------------------

(provide 'my-web-access)

