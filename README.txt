This directory is for the development of the Crypter wrapper
(which is both applet and application)

tag with
    
	CRYPTER_REL_x_x_x

(no revison tag).

The standard realization of a CrytoModule.java should actually
be an instance of a crypto-INTERFACE; this is not programmed
this way to keep things simple.

The standard realization is an implementation of the
Cramer-Shoup'98 Crypto System.




The KeyGeneratorCS.java:
  used to create the source code for some CrytoModule.java
  with a new secret/public key pair.

The more bits the key, the longer it (probably) takes.
However, the times seem to be moderate even for longer keys:

Creation of MachoCryptoModule6000.java: (6014 bit)
real    332m16.595s
user    294m0.300s
sys     4m5.600s

Creation of KeyModule5000.java: (a 5041 bit key)

real    152m29.489s
user    125m1.790s
sys     0m0.210s

For 1000 bit keys:
real    2m23.387s
user    1m53.040s
sys     0m0.370s

real    0m17.813s
user    0m17.250s
sys     0m0.150s

real    0m41.148s
user    0m40.200s
sys     0m0.090s

For 2000 bit keys:
real    16m30.696s
user    15m14.900s
sys     0m0.230s

real    13m51.850s
user    13m8.080s
sys     0m0.210s

real    4m43.446s
user    4m34.130s
sys     0m0.850s

real    7m33.858s
user    7m32.740s
sys     0m0.150s

real    4m2.024s
user    2m17.960s
sys     0m0.150s

