This repository contains a Java implementation of the Cramer Shoup'98
Crypto System, usable both as applet and application.

-----------------------------------------
Usage instructions are in file HOWTO.txt
For a quick try-out of key generation and en/decryption, call
    make -C src check
-----------------------------------------
Shortcomings:
  The standard realization of a CrytoModule.java should actually
  be an instance of a crypto-INTERFACE; this is not programmed
  this way to keep things simple.
-----------------------------------------
Find out more at: http://www.verify-it.de/sub/crypter.html
-----------------------------------------

have fun! 
<omoeller@verify-it.de> 


