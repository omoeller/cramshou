// -*- mode: JDE; c-basic-offset: 2; -*-
// /////////////////////////////////////////////////////////////
// Key Generation Algorithm
//
// Synopsis:
//  Crypter, a realizatin with the Cramer Shoup Cryptosystem  
// /////////////////////////////////////////////////////////////
// @TABLE OF CONTENTS:		       [TOCD: 19:02 11 Jul 2002]
//
//  [1] Cryptographic Functions
//      [1.1] Create New prime p
//  [2] Print Results
// ==========================================================
// @FILE:    KeyGeneratorCS.java
// @PLACE:   Mars Workstation
// @FORMAT:  java
// @AUTHOR:  M. Oliver M'o'ller     <omoeller@verify-it.de>
// @BEGUN:   Thu Jul 11 10:59:06 2002
// @VERSION: $Revision: 1.4 $ 		$Date: 2004/07/12 20:51:50 $
// /////////////////////////////////////////////////////////////
// $Id: KeyGeneratorCS.java,v 1.4 2004/07/12 20:51:50 oli Exp $
// /////////////////////////////////////////////////////////////
// @SPELL:   british 			Wed Jul  7 07:43:06 2004

import java.lang.*;

import java.math.BigInteger;

import java.util.Vector;
import java.util.Random;
import java.util.Date;

import java.text.DateFormat;

import java.io.File;
import java.io.FileWriter;
import java.io.OutputStreamWriter;



//**** from other packages 

//****************************************

/**

 *
 * @author <A HREF="MAILTO:omoeller@verify-it.de?subject=KeyGeneratorCS.java%20(1.0alpha%20Sun%20Jul%2014%2023:44:24%202002)">M. Oliver M&ouml;ller</A>
 * @version 1.0alpha Sun Jul 14 23:45:35 2002
 */
public class KeyGeneratorCS  {

  // //////////////////////////////////////////////////////////////////////
  // ////////////////////////////// FIELDS ////////////////////////////////
  // //////////////////////////////////////////////////////////////////////

  /**
   * Name of this version (should be global for project)
   */
  public static final String VERSION_NAME = "1.1 $Revision: 1.4 $";

  /**
   * Date of last changes
   */
  public static final String VERSION_DATE = "$Date: 2004/07/12 20:51:50 $";

  /**
   * String describing the version of this API
   */
  public static final String VERSION = VERSION_NAME + " " + VERSION_DATE;


  /**
   * Certainty that a used pseudoprime is actually prime
   * (probability (1/2)^certainty of failure)
   */
  private static int certainty = 100;

  /**
   * Name and email of the owner
   */
  private static String owner = "nobody <X@nowhere.com>";

  /**
   * Name of the geneated key
   */
  private static String keyname = "Chocklate";
  

  /**
   * Number of bits in prime p (primeP)
   */
  private static int nbits = -1;
  

  /**
   * Large modolus Integer
   */
  private static BigInteger primeP;


  /**
   * Generator for the ring modulo primeP
   */
  private static BigInteger g1;

  /**
   * Generator for the ring modulo primeP
   */
  private static BigInteger g2;
  
  /**
   * Generator for the ring modulo primeP
   */
  private static BigInteger hg1;
  
  /**
   * Generator for the ring modulo primeP
   */
  private static BigInteger hg2;
  

  /**
   * Other numbers used in Key Generation
   */
  private static BigInteger c,d,h,x1,x2,y1,y2,z;
  

  /**
   * Array of prime factors of (p-1), mixed order
   */
  private static BigInteger[] pMinus1factorization;
  

  /**
   * Array of exponents for the prime factors of (p-1),
   * ordered as in pMinus1factorization
   */
  private static int[] pMinus1factorizationExponents;


  /**
   * Output Filename (optional)
   */
  static String filename = null;


  static long[] initialPrimes = {2, 3, 5, 7, 11, 13, 17, 19, 23 
  } ;

  private static Vector primes;

  private static int primePointer;
  
  
  private static BigInteger const2 =  new BigInteger("2");
  private static BigInteger const3 =  new BigInteger("3");
  private static BigInteger const4 =  new BigInteger("4");
  private static BigInteger const5 =  new BigInteger("5");
  private static BigInteger const7 =  new BigInteger("7");
  private static BigInteger const9 =  new BigInteger("9");
  private static BigInteger const11 = new BigInteger("11");
  private static BigInteger const13 = new BigInteger("13");
  private static BigInteger const17 = new BigInteger("17");
  private static BigInteger const19 = new BigInteger("19");
  private static BigInteger const23 = new BigInteger("23");
  private static BigInteger const256 = new BigInteger("256");
  private static BigInteger const2310 = new BigInteger("2310");

  

  // //////////////////////////////////////////////////////////////////////
  // //////////////////////////  CONSTRUCTORS  ////////////////////////////
  // //////////////////////////////////////////////////////////////////////

  /**
   * Default Constructor
   */
  public KeyGeneratorCS() {
  }
  
  public static void main(String argv [])
    throws java.io.IOException {
    int i = 0;
    int bits = -1;
    
    if(0 == argv.length)
      abort();

    while(i< argv.length){
      if(argv[i].charAt(0) == '-'){
	if(1 == (argv[i]).length() )
	  abort();

	switch (argv[i].charAt(1))  {

	case 'f' : 
	  if(++i == argv.length)
	    abort();
	  filename = argv[i];
	  break;

	case 'n' : 
	  if(++i == argv.length)
	    abort();
	  keyname = argv[i];
	  break;

	case 'o' : 
	  if(++i == argv.length)
	    abort();
	  owner = argv[i];
	  break;

 	case 'c' : 
	  if(++i == argv.length)
	    abort();
	  certainty = (new Integer(argv[i])).intValue();
	  if(certainty <1) 
	    abort();
	  break;

	case 'h' : 
	  printUsage();
	  printDescription();
	  System.exit(0);
	  break;

	default : 
	  System.out.println("ERROR: unexpected option    " + argv[i]);
	  abort();

	}
      }
      else {
	// setting random bits
	if(bits < 0)
	  bits = (new Integer(argv[i])).intValue();
	else
	  abort();
      }
      i++;
    }
    if(bits <= 0) 
      abort();
    
      System.out.println("** Number of random bits:    " + bits );
    if( null != filename )
      System.out.println("** Output filename      :    " + filename );
    else
      System.out.println("** Output               :    stdout");

      System.out.println("** Key-Name Prefix      :    " + keyname );
      System.out.println("** Owner                :    " + owner );
      System.out.println("** Certainty            :    " + certainty );
      System.out.println();
      
    primes = new Vector();
    for(i = 0; i < initialPrimes.length; i++){
      primes.addElement(new BigInteger((new Long(initialPrimes[i])).toString()));
    }

    
    OutputStreamWriter sw;

    if( null != filename){
      File f = new File(filename);    
      if(new File(filename).exists()){
	System.out.println("++ Error: file \"" +
		       filename +
		       "\"exists.");
	System.exit(0);		
      }
      sw = new FileWriter(f);
    }
    else {
      sw = new OutputStreamWriter(System.out);
    }
  
    createNewKey(bits);

    outputCryptoModule(sw);
    
  }
    
  /**
   * Explain the usage (command line)
   */
  private static void printUsage(){
    System.out.println("KeyGeneratorCS V " + VERSION + "   <omoeller@verify-it.de>\n");
    System.out.println("USAGE:   java KeyGeneratorCS [OPTIONS] NBITS\n");
    System.out.println("  NBITS:   (minimal) number of random bits in key");
    System.out.println("  OPTIONS: ");
    System.out.println("   -f FILENAME   write the new CrytoModule to file");
    System.out.println("   -n NAME       name of the generated key [default: " + keyname + "]");
    System.out.println("   -o OWNER      name+email of owner [default: " + owner +"]");
    System.out.println("   -c CERTAINTY  risk of guessing a bad prime is (1/2)^c  [default: " + certainty +"]");
										    System.out.println("   -h            display help");

  }

  /**
   * Explain the usage (command line) and EXIT
   */
  private static void abort(){
    printUsage();
    
    System.exit(0);
  }
  
  
  /**
   * Print the description
   */
  private static void printDescription(){
    
    System.out.println("");
    System.out.println("DESCRIPTION:");
    System.out.println("");
    System.out.println("  This serves as the key-generation algorithm for the Crypter applet.");
    System.out.println("  Actually it creates both the key and wraps it into a CryptoModule.");
    System.out.println("  To use this properly, do the following:");
    System.out.println("");
    System.out.println("  1) create a file CryptoModule.java, e.g., via");
    System.out.println("");
    System.out.println("     java KeyGeneratorCS -f CryptoModule.java -n \"YOUR NAME <EMAIL>\" 2030");
    System.out.println("");
    System.out.println("  2) compile the file CryptoModule.java in the same directory as you");
    System.out.println("     have Crypter.java");
    System.out.println("");
    System.out.println("     javac CryptoModule.java Crypter.java");
    System.out.println("");
    System.out.println("  3) You can now use the command line to encrypt and decrypt:");
    System.out.println("");
    System.out.println("     java Crypter e MESSAGEFILE CRYPTOFILE");
    System.out.println("     java Crypter d CRYPTOFILE  DECRYPTED-MESSAGE");
    System.out.println("");
    System.out.println("  IMPORTANT NOTES:");
    System.out.println("  - if you want to make the Crypto-Applet available for online encryption,");
    System.out.println("    then copy CryptoModule.java to a separate directory and");
    System.out.println("    replace all the SECRET KEY parts in CryptoModule.java with { 0 } .");
    System.out.println("    This version of the CryptoModule can then encrypt, but not decrypt.");
    System.out.println("    Never make your secret key available to anyone!");
    System.out.println("");
    System.out.println("  - if you use very small keys (< 500), the system is very vulnerable to");
    System.out.println("    brute force attacks. There is a trade off between security and");
    System.out.println("    time it takes to encrypt/decrypt. Keys with at least 1000 random bits");
    System.out.println("    are recommended.");
    System.out.println("");
    System.out.println("");
    System.out.println("  You can find an example usage of the encryption via a web-browser on");
    System.out.println("      http://www.verify-it.de/sub/crypter.html");
    System.out.println("");
  }
  

  // //////////////////////////////////////////////////////////////////////
  // ///////////////////////////// METHODS  ///////////////////////////////
  // //////////////////////////////////////////////////////////////////////


  // =================================================================
  // [1] Cryptographic Functions
  // =================================================================

  /**
   *  Creates the complete key
   */
  private static void createNewKey(int bits){
    
    System.out.println("-- creating new key with minimum " + bits + " random bits ------------------");  

    createPrimePair(bits);
    showFactorization();
    
    create4Generator();
    showGenerators();

    Random rnd = new Random();

    do {  x1 = (new BigInteger(nbits, rnd)).mod(primeP); }
    while(x1.equals(BigInteger.ZERO));
    do {  x2 = (new BigInteger(nbits, rnd)).mod(primeP); }
    while(x2.equals(BigInteger.ZERO));
    do {  y1 = (new BigInteger(nbits, rnd)).mod(primeP); }
    while(y1.equals(BigInteger.ZERO));
    do {  y2 = (new BigInteger(nbits, rnd)).mod(primeP); }
    while(y2.equals(BigInteger.ZERO));
    do {  z  = (new BigInteger(nbits, rnd)).mod(primeP); }
    while(z.equals(BigInteger.ZERO));

    c = ((g1.modPow(x1,primeP)).multiply(g2.modPow(x2,primeP))).mod(primeP);
    d = ((g1.modPow(y1,primeP)).multiply(g2.modPow(y2,primeP))).mod(primeP);

    h = g1.modPow(z, primeP);

    System.out.println("-- key creation finished --------------------------------------------");  


  }
    

  /**
   * computes a bigger (strong pseudoprim)-number than n
   * uses Miller-Rabin-Test 
   * k reflects the number of bits in n 
   * (neccessary, for the log of n is not evaluable any more)
   * 
   * This function not only computes a prime p but also finds
   * a prime  p = r*q + 1 for a small r. 
   */
  private static void createPrimePair(int bits){

    if(bits < 10){
      System.out.println(">> Number of given bits too small.");
      System.exit(0);
    }
    else { 
      
      // =========================================
      // [1.1] Create New prime p
      // =========================================

      Random rnd = new Random();

      BigInteger primeQ = new BigInteger(bits, certainty, rnd);

      while(
	    (primeQ.mod(const3)).equals(BigInteger.ZERO) ||
	    (primeQ.mod(const5)).equals(BigInteger.ZERO) ||
	    (primeQ.mod(const7)).equals(BigInteger.ZERO) ||
	    (primeQ.mod(const9)).equals(BigInteger.ZERO) ||
	    (primeQ.mod(const11)).equals(BigInteger.ZERO) ||
	    (primeQ.mod(const13)).equals(BigInteger.ZERO) ||
	    (primeQ.mod(const17)).equals(BigInteger.ZERO) ||
	    (primeQ.mod(const19)).equals(BigInteger.ZERO) ||
	    (primeQ.mod(const23)).equals(BigInteger.ZERO) 
	    ){
	primeQ = new BigInteger(bits, certainty, rnd);
      }
      System.out.println("Start with: " + primeQ.toString());

      Vector shiftCandidates = new Vector();

      shiftCandidates.addElement((primeQ.multiply(const2)).add(BigInteger.ONE));
      
      BigInteger numberP = primeQ.mod(const2310);

      BigInteger shift;
      BigInteger bigI;
      int i;

      for(i=2; i < 2310; i++){
	bigI = (new BigInteger((new Integer(i)).toString()));
	shift = (numberP.multiply(bigI));
	if( 
	   ((shift.mod(const3)).equals(BigInteger.ONE)) ||
	   ((shift.mod(const5)).equals(const2)) ||
	   ((shift.mod(const7)).equals(const3)) ||
	   ((shift.mod(const11)).equals(const5))
	   ){
	  // skip
	}
	else {
	  shiftCandidates.addElement(((primeQ.multiply(bigI)).multiply(const2)).subtract((BigInteger)(shiftCandidates.elementAt(shiftCandidates.size() - 1))));
	}
      }
      
      System.out.println("Actual 2310 grid size: " + shiftCandidates.size());
      

      BigInteger longJ = BigInteger.ONE;
      boolean notfound = true;
      
      while(notfound){
	System.out.print("=");

	numberP = ((longJ.multiply(primeQ)).multiply(const2)).add(BigInteger.ONE);
	longJ = longJ.add(const2310);

	i = 1;
	
	while(i < shiftCandidates.size()){
	  if(numberP.isProbablePrime(certainty)){
	    notfound = false;
	    i = shiftCandidates.size(); 
	  }
	  else {
	    numberP = numberP.add((BigInteger)shiftCandidates.elementAt(i++));
	    System.out.print("+");
	  }
	}
      }
      
      System.out.println("");
      System.out.println("Found: p = " + numberP.toString());
      System.out.println("Found: q = " + primeQ.toString());

      nbits = numberP.bitLength();

      primeP = numberP;

      // -- Factorizing p-1 ------------------------------------------------
  
      BigInteger bigN = (numberP.subtract(BigInteger.ONE)).divide(primeQ);
    
      Vector pMinus1Factors = new Vector();
      Vector pMinus1FactorMultiplicity = new Vector();

      pMinus1Factors.addElement(primeQ);
      pMinus1FactorMultiplicity.addElement(new Integer(1));

      initPrimes();

      BigInteger pp = nextSmallestPrime();
      
      while(!bigN.equals(BigInteger.ONE)){
	if((bigN.mod(pp)).equals(BigInteger.ZERO)){
	  int j = 1;
	  bigN = bigN.divide(pp);
	  while( (bigN.mod(pp)).equals(BigInteger.ZERO) ){
	    bigN = bigN.divide(pp);
	    j++;
	  }
	    pMinus1Factors.addElement(pp);
	    pMinus1FactorMultiplicity.addElement((new Integer(j)));
	    System.out.print("/");
	}
	pp = nextSmallestPrime();
      }
      System.out.println();

      pMinus1factorization = new BigInteger[pMinus1Factors.size()];
      pMinus1factorizationExponents = new int[pMinus1Factors.size()];

      for(i = 0; i < pMinus1Factors.size(); i++){
	pMinus1factorization[i] = (BigInteger)pMinus1Factors.elementAt(i);
	pMinus1factorizationExponents[i] = ((Integer)pMinus1FactorMultiplicity.elementAt(i)).intValue();
      }

    }
  } 

  /**
  * generates a 4-tuple (g1 g2 hg1 hg2) where
  * g1, g2, hg1, hg2  are generators of Z_p^*
  * This implementation relying on ORDinG and PRIMEL is due to 
  * L"uneburg, Heinz: On the rational normal form of endomorphisms. A primer
  * to constructive algebra. Mannheim/Wien/Zuerich: B.I.-Wissenschaftsverlag.
  * 1987, chapter XIII.
  * 
  * In addition, it projects generators down to (2... p/2) -
  * If g is a generator, then also -g is (!)
  */
  private static void create4Generator(){

    System.out.print("\n>> Guessing g1 ");
    g1 = projectDown(primel(), primeP);
    do {
      System.out.print("\n>> Guessing g2 ");
      g2 = projectDown(primel(), primeP);
    } while (g2.equals(g1));
    do {
      System.out.print("\n>> Guessing hg1 ");
      hg1 = projectDown(primel(), primeP);
    } while (hg1.equals(g1) || hg1.equals(g2));
    do {
      System.out.print("\n>> Guessing hg2 ");
      hg2 = projectDown(primel(), primeP);
    } while (hg2.equals(g1) || hg2.equals(g2) || hg2.equals(hg1) );
  }

  /**
   * computes order of a element
   */
  private static BigInteger ordInG(BigInteger x){
    
    BigInteger ord = primeP.subtract(BigInteger.ONE);
    int j = 0;
    
    for(int i = 0; i < pMinus1factorization.length; i++){
      System.out.print("*");
      for(j = 1; j < pMinus1factorizationExponents[i]; j++){
	if(x.modPow((ord.divide(pMinus1factorization[i])),primeP).equals(BigInteger.ONE)
	   ){
	  System.out.print("@");
	  ord = ord.divide(pMinus1factorization[i]);
	}
      }
      if(x.modPow(ord.divide(pMinus1factorization[i]),primeP).equals(BigInteger.ONE)){
	ord = ord.divide(pMinus1factorization[i]);
      }
    }
    return ord;
  }

  /**
   * Return a generator of ring primeP
   */
  private static BigInteger primel(){

    try {

      BigInteger p1 = primeP.subtract(BigInteger.ONE);
      BigInteger p3 = primeP.subtract(const3);
      BigInteger prim =
      ((new BigInteger(nbits, new Random())).mod(p3)).add(const2); 
      BigInteger ord = ordInG(prim);
      BigInteger y;
      BigInteger ordy;
      BigInteger c;
      BigInteger s;
      BigInteger ss;
      
      
      System.out.print("--ok--");
      while(-1 == ord.compareTo(primeP.subtract(BigInteger.ONE))){
	y = ((new BigInteger(nbits, new Random())).mod(p3)).add(const2); 
	ordy = ordInG(y);
	c = ordy.gcd(ord);
	s = ord.mod(ordy.divide(c));
	
	if(ordy.equals(p1)){
	  prim = y;
	  ord = ordy; 
	}
	else {
	
	  if ( (-1 == c.compareTo(ordy)) &&
	       (-1 == (BigInteger.ZERO).compareTo(s))
	     ){
	    ss = ordy.mod(ord.divide(c));
	    ss = ss.divide(ss.gcd(s));
	    prim = prim.modPow(ord.divide(s), 
			       primeP);
	    prim = (prim.multiply(y.modPow(ordy.divide(ss),
					   primeP))
		    ).mod(primeP);
	  }
	}
      }
      return prim;
    }
    catch (java.lang.ArithmeticException e){
      // sometimes has problems with division byte 0... RESTART in this case
      System.out.print("REDO");
      return primel();
    }
  }

  /**
   * Take Generator from the lower half
   */
  private static BigInteger projectDown(BigInteger gen, BigInteger p){
  if(-1 == (p.divide(const2)).compareTo(gen))
    return p.subtract(gen);
  else 
    return gen;
  }
  

  /**
   * Initialize nextSmallestPrime
   */
  private static void initPrimes(){
      primePointer = 0;
  }
  

  
  private static BigInteger nextSmallestPrime(){
    if(primePointer < primes.size() )
      return (BigInteger)primes.elementAt(primePointer++);
    else {
      BigInteger newPrime = ((BigInteger)primes.lastElement()).add(const2);
      int i = 1;
      while(-1 == ((((BigInteger)primes.elementAt(i-1)).multiply((BigInteger)primes.elementAt(i-1))).compareTo(newPrime))){
	if(newPrime.mod((BigInteger)primes.elementAt(i)).equals(BigInteger.ZERO)){
	  newPrime = newPrime.add(const2);
	  System.out.print("#");
	  i = 1;
	}
	else {
	  i++;
	}
      }
      
      System.out.print(newPrime.toString());
      primes.addElement(newPrime);
	    
      return newPrime;
    }
  }



  // =================================================================
  // [2] Print Results
  // =================================================================

  /**
   * Show the computed Factorizations
   */
  private static void showFactorization(){
    int i;
    System.out.println(primeP.subtract(BigInteger.ONE).toString() + " = " );
    for(i = 0; i < pMinus1factorization.length; i++){
      System.out.print(pMinus1factorization[i].toString());
      System.out.print("^" + pMinus1factorizationExponents[i]);
      if(i+1 <  pMinus1factorization.length)
	System.out.print(" * ");
      System.out.println();
    }
  }

  /**
   * Show Generators
   */
  private static void showGenerators(){
    System.out.println("\ng1: " + g1.toString());
    System.out.println("g2: " + g2.toString());
    System.out.println("hg1: " + hg1.toString());
    System.out.println("hg2: " + hg2.toString());
  }


  /**
   * Convert a BigInteger into a list of bytes
   */
  private static String bytelist(BigInteger big){
    StringBuffer sb = new StringBuffer();
    BigInteger n = big;
    byte b;
    do {
      if(sb.length() > 0)
	sb.insert(0, ", ");
      b = (byte)((n.mod(const256)).intValue());
      n = n.divide(const256);
      if(n.equals(BigInteger.ZERO)){
	if (b < 0)
	  sb.insert(0, "0, " + b);
	  else
	    sb.insert(0, b );
	break;
      }
      else {
	sb.insert(0, b );
      }
    } while(true);

    return sb.toString();
  }


  /**
   * write the complete CryptoModlue to a stream
   */
  public static void outputCryptoModule(OutputStreamWriter os)
    throws java.io.IOException {

    os.write("/************************************************************\n");
    os.write(" * @(#)CryptoModule.java\n");
    os.write(" * \n");
    os.write(" * Generated by KeyGeneratorCS V " + VERSION + "\n");
    os.write(" * Created : " + DateFormat.getDateTimeInstance().format(new Date()) + "\n");
    os.write(" *\n");
    os.write(" * Requires: java 1.1.5 (or higher)\n");
    os.write(" * \n");
    os.write(" * \n");
    os.write(" *           [as e.g. provided in Netscape 4.06]\n");
    os.write(" *\n");
    os.write(" * References:\n");
    os.write(" * \n");
    os.write(" * [CS98] Ronald Cramer and Victor Shoup: A practical public key crypto\n");
    os.write(" *        system provably secure against adaptive chosen  ciphertext attack\n");
    os.write(" *        in proceedings of Crypto 1998, LNCS 1462, p.13ff  \n");
    os.write(" * \n");
    os.write(" * No Copyright (!c)\n");
    os.write(" */\n");
    os.write("\n");
    os.write("\n");
    os.write("import java.math.BigInteger;\n");
    os.write("import java.util.Random;\n");
    os.write("\n");
    os.write("  /**\n");
    os.write("   * CryptoModule implements the Cramer-Shoup \n");
    os.write("   * public key Crypto Algorithm\n");
    os.write("   *\n");
    os.write("   * If you want to use it, you can find more information on:\n");
    os.write("   * <PRE>\n");
    os.write("   * see: http://www.verify-it.de/sub/cramer_shoup.html\n");
    os.write("   *      http://www.verify-it.de/sub/crypter.html\n");
    os.write("   *      http://www.verify-it.de/applet/KeyGeneratorCS.java\n");
    os.write("   * </PRE>\n");
    os.write("   * \n");
    os.write("   */\n");

    os.write("public class CryptoModule {\n");
    os.write("\n");
    os.write("    /************************************************************\n");
    os.write("     * Public and secret keys\n");
    os.write("     ************************************************************/\n");
    os.write("    \n");
    os.write("  public String keyNameString = \"Method:   Cramer-Shoup98\\nKey-Name: " + keyname + "-" + nbits + "\\nOwner:    " + owner +"\\n\";\n");
    os.write("  \n");
    os.write("  private SecretKey sk;\n");
    os.write("  private PublicKey pk;\n");
    os.write("  \n");
    os.write("  private Random rnd;\n");
    os.write("  \n");
    os.write("  public CryptoModule(){\n");
    os.write("    \n");
    os.write("    /****************************************\n");
    os.write("     * Initialize:\n");
    os.write("     * Set the keys to personal adjustings\n");
    os.write("     ****************************************/\n");
    os.write("    \n");
    os.write("    /*\n");
    os.write("     * Method:   Cramer-Shoup98\n");
    os.write("     * Key-Name: " + keyname + "-" + nbits + "\n");
    os.write("     * Owner:    " + owner   + "\n");
    os.write("     */\n");
    os.write("    \n");
    os.write("    byte[] p = { " + bytelist(primeP) + " };\n");
    os.write("    byte[] hg1 = { " + bytelist(hg1) + " };\n");
    os.write("    byte[] hg2 = { " + bytelist(hg2) + " };\n");
    os.write("    \n");
    os.write("    byte[] g1 = { " + bytelist(g1) + " };\n");
    os.write("    byte[] g2 = { " + bytelist(g2) + " };\n");
    os.write("    \n");
    os.write("    \n");
    os.write("    byte[] c = { " + bytelist(c) + " };\n");
    os.write("    byte[] d = { " + bytelist(d) + " };\n");
    os.write("    byte[] h = { " + bytelist(h) + " };\n");
    os.write("    \n");
    os.write("    /* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
    os.write("     * SECRET KEY components start here [replace via { 0 }] \n");
    os.write("     * !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
    os.write("     */\n");
    os.write("    byte[] x1 = { " + bytelist(x1) + " };\n");
    os.write("    byte[] x2 = { " + bytelist(x2) + " };\n");
    os.write("    byte[] y1 = { " + bytelist(y1) + " };\n");
    os.write("    byte[] y2 = { " + bytelist(y2) + " };\n");
    os.write("    byte[] z =  { " + bytelist(z) + " };\n");
    os.write("    //****************************************\n");
    os.write("    \n");
    os.write("    pk = new PublicKey(" + nbits + ",p,g1,g2,c,d,h,hg1,hg2);\n");
    os.write("    sk = new SecretKey(" + nbits + ",p,x1,x2,y1,y2,z,hg1,hg2);\n");
    os.write("    \n");
    os.write("    \n");
    os.write("    \n");
    os.write("    /****************************************\n");
    os.write("     * Initiatize random generator\n");
    os.write("     ****************************************/\n");
    os.write("    \n");
    os.write("    rnd = new Random();\n");
    os.write("  }\n");
    os.write("  \n");
    os.write("  \n");
    os.write("  /************************************************************\n");
    os.write("   * Converter Methods\n");
    os.write("   ************************************************************/\n");
    os.write("  \n");
    os.write("  private BigInteger bits2BigInteger(boolean[] bits){\n");
    os.write("    int len = bits.length;\n");
    os.write("    byte[] bytes = new byte[2 + (len/8)];\n");
    os.write("    int b;\n");
    os.write("    int index = 1+ len/8;\n");
    os.write("    int bitIndex = 0;\n");
    os.write("    \n");
    os.write("    // in bits:  least significant bit comes first\n");
    os.write("    // in bytes: most significant bit is first!\n");
    os.write("    \n");
    os.write("    bytes[0] = 0;  // play safe - we do not want a negative number...\n");
    os.write("    bytes[1] = 0;\n");
    os.write("    \n");
    os.write("    while(bitIndex<len){\n");
    os.write("      b = 0;\n");
    os.write("      if((bitIndex<len)&&(bits[bitIndex++]))b = b|1;\n");
    os.write("      if((bitIndex<len)&&(bits[bitIndex++]))b = b|2;\n");
    os.write("      if((bitIndex<len)&&(bits[bitIndex++]))b = b|4;\n");
    os.write("      if((bitIndex<len)&&(bits[bitIndex++]))b = b|8;\n");
    os.write("      if((bitIndex<len)&&(bits[bitIndex++]))b = b|16;\n");
    os.write("      if((bitIndex<len)&&(bits[bitIndex++]))b = b|32;\n");
    os.write("      if((bitIndex<len)&&(bits[bitIndex++]))b = b|64;\n");
    os.write("      if((bitIndex<len)&&(bits[bitIndex++]))b = b|128;\n");
    os.write("      bytes[index--] = (byte)b;\n");
    os.write("    }\n");
    os.write("    return new BigInteger(bytes);\n");
    os.write("  }\n");
    os.write("  \n");
    os.write("  private boolean[] bigInteger2bits(int k, BigInteger b){\n");
    os.write("    boolean[] bits = new boolean[k];\n");
    os.write("    int i;\n");
    os.write("    \n");
    os.write("    for(i = 0; i< k; i++)\n");
    os.write("      bits[i] = b.testBit(i);\n");
    os.write("    return bits;\n");
    os.write("  }\n");
    os.write("  \n");
    os.write("  \n");
    os.write("  /************************************************************\n");
    os.write("   * Hashing\n");
    os.write("   ************************************************************/\n");
    os.write("  \n");
    os.write("  private BigInteger hashOnOneNumber(BigInteger p,\n");
    os.write("				     BigInteger hg1,\n");
    os.write("				     BigInteger hg2,\n");
    os.write("				     BigInteger nn){\n");
    os.write("    BigInteger q = p.shiftRight(1); // -1 div 2\n");
    os.write("    return hashOnTwoNumbers(p,hg1,hg2,nn.divide(q),nn.mod(q));\n");
    os.write("  }\n");
    os.write("  private BigInteger hashOnTwoNumbers(BigInteger p,\n");
    os.write("				      BigInteger hg1,\n");
    os.write("				      BigInteger hg2,\n");
    os.write("				      BigInteger a1,\n");
    os.write("				      BigInteger a2){\n");
    os.write("    return ((hg1.modPow(a1,p)).multiply(hg2.modPow(a2,p))).mod(p);\n");
    os.write("  }\n");
    os.write("  \n");
    os.write("  private BigInteger hashBitList(int k,\n");
    os.write("				 BigInteger p,\n");
    os.write("				 HashFunction hash,\n");
    os.write("				 boolean[] bits){\n");
    os.write("    BigInteger hg1 = hash.hash_g1;\n");
    os.write("    BigInteger hg2 = hash.hash_g2;\n");
    os.write("    \n");
    os.write("    int m   = k+k-2;\n");
    os.write("    int len = m - k - 1; // the length of ONE hash component\n");
    os.write("    int lengthDeficit = len - (bits.length % len);\n");
    os.write("    int index = - lengthDeficit; \n");
    os.write("    int i;\n");
    os.write("    BigInteger arg;\n");
    os.write("    byte[] zero = {0};\n");
    os.write("    BigInteger res = new BigInteger(zero);\n");
    os.write("    BigInteger e;\n");
    os.write("    boolean[] transferList = new boolean[len];\n");
    os.write("    \n");
    os.write("    while(index < bits.length){\n");
    os.write("      // copy bits\n");
    os.write("      for(i = 0; i < len; i++){\n");
    os.write("	if(index < 0)\n");
    os.write("	  transferList[i] = false;\n");
    os.write("	else\n");
    os.write("	  transferList[i] = bits[index];\n");
    os.write("	index++;}\n");
    os.write("      \n");
    os.write("      arg = bits2BigInteger(transferList).add((res.shiftLeft(1)).setBit(0).shiftLeft(len));\n");
    os.write("      res = hashOnOneNumber(p, hg1, hg2, arg);\n");
    os.write("    }\n");
    os.write("    return res;\n");
    os.write("  }\n");
    os.write("  \n");
    os.write("  /************************************************************\n");
    os.write("   * Auxillary\n");
    os.write("   ************************************************************/\n");
    os.write("  \n");
    os.write("  private BigInteger bigRandom(int bits){\n");
    os.write("    // there should be a constructor with signature\n");
    os.write("    // BigInteger(int bits, Random rnd) that does that job....\n");
    os.write("    // only my compiler cannot find it for some reason.\n");
    os.write("    int nbytes = 2+bits/8;\n");
    os.write("    int i;\n");
    os.write("    int b;\n");
    os.write("    byte[] bytes = new byte[nbytes];\n");
    os.write("    \n");
    os.write("    for(i=1; i< nbytes; i++){\n");
    os.write("      b = (rnd.nextInt()) % 256;\n");
    os.write("      if(b>127)b = b - 256;\n");
    os.write("      bytes[i] = (byte)b;}\n");
    os.write("    bytes[0] = 1;\n");
    os.write("    \n");
    os.write("    return new BigInteger(bytes);\n");
    os.write("  };\n");
    os.write("  \n");
    os.write("  private boolean[] bitListOne(int k,\n");
    os.write("			       BigInteger b1){\n");
    os.write("    // LEAST significant bits first...\n");
    os.write("    boolean[] res = new boolean[k];\n");
    os.write("    int i;\n");
    os.write("    int index = 0;\n");
    os.write("    \n");
    os.write("    for(i = 0; i <k ; i++)\n");
    os.write("      res[index++] = b1.testBit(i);\n");
    os.write("    return res;\n");
    os.write("  }\n");
    os.write("  \n");
    os.write("  private boolean[] bitListThree(int k,\n");
    os.write("				 BigInteger b1,\n");
    os.write("				 BigInteger b2,\n");
    os.write("				 BigInteger b3){\n");
    os.write("    // LEAST significant bits first...\n");
    os.write("    boolean[] res = new boolean[3*k];\n");
    os.write("    int i;\n");
    os.write("    int index = 0;\n");
    os.write("    \n");
    os.write("    for(i = 0 ; i < k; i++)\n");
    os.write("      res[index++] = b1.testBit(i);\n");
    os.write("    for(i = 0 ; i < k; i++)\n");
    os.write("      res[index++] = b2.testBit(i);\n");
    os.write("    for(i = 0 ; i < k; i++)\n");
    os.write("      res[index++] = b3.testBit(i);\n");
    os.write("    \n");
    os.write("    return res;\n");
    os.write("  }\n");
    os.write("  private boolean[] bitListFour(int k,\n");
    os.write("				  BigInteger b1,\n");
    os.write("				  BigInteger b2,\n");
    os.write("				  BigInteger b3,\n");
    os.write("				  BigInteger b4){\n");
    os.write("	// least significant bits first...\n");
    os.write("	boolean[] res = new boolean[4*k];\n");
    os.write("	int i;\n");
    os.write("	int index = 0;\n");
    os.write("\n");
    os.write("	for(i = 0 ; i < k; i++)\n");
    os.write("	    res[index++] = b1.testBit(i);\n");
    os.write("	for(i = 0 ; i < k; i++)\n");
    os.write("	    res[index++] = b2.testBit(i);\n");
    os.write("	for(i = 0 ; i < k; i++)\n");
    os.write("	    res[index++] = b3.testBit(i);\n");
    os.write("	for(i = 0 ; i < k; i++)\n");
    os.write("	    res[index++] = b4.testBit(i);\n");
    os.write("\n");
    os.write("	return res;\n");
    os.write("    }\n");
    os.write("\n");
    os.write("    /************************************************************\n");
    os.write("     * --  Encrypting  --\n");
    os.write("     ************************************************************/\n");
    os.write("    \n");
    os.write("	public boolean[] encrypt(boolean[] message){\n");
    os.write("	/***\n");
    os.write("	 * Returns a (big) array representing the\n");
    os.write("	 * concatenation of encryption blocks\n");
    os.write("	 *        [u1 u2 e v]\n");
    os.write("	 * Since the (bit-)length of each component of the block is given\n");
    os.write("	 * with k, it is not necessary to encapsulate those additionally.\n");
    os.write("	 **************************************************/\n");
    os.write("	    \n");
    os.write("	    int messageLength = message.length;\n");
    os.write("	    int el            = pk.k - 1; // one 'bit' is not complete \n");
    os.write("	    int i;\n");
    os.write("	    int toEncrypt     = (1+ (message.length / el))*pk.k*4;\n");
    os.write("	    boolean[] mChunk  = new boolean[pk.k];\n");
    os.write("	    mChunk[pk.k-1] = false;      // ignore most significant bit\n");
    os.write("	    boolean[] cChunk  = new boolean[pk.k*4];\n");
    os.write("	    boolean[] res     = new boolean[toEncrypt];\n");
    os.write("	    int resIndex      = 0;\n");
    os.write("	    \n");
    os.write("	    BigInteger r,u1,u2,e,m,alpha,v;\n");
    os.write("\n");
    os.write("	    int pointer = 0;\n");
    os.write("	    int index = 0;\n");
    os.write("\n");
    os.write("\n");
    os.write("	    while(pointer < messageLength){\n");
    os.write("		index = 0;\n");
    os.write("		while((index < el)&&(pointer < messageLength))\n");
    os.write("		    mChunk[index++] = message[pointer++];\n");
    os.write("		while(index < el) // fill with random bits\n");
    os.write("		    mChunk[index++] = ( (rnd.nextInt() & 1) == 1);\n");
    os.write("		m  = bits2BigInteger(mChunk);\n");
    os.write("\n");
    os.write("		r = bigRandom(pk.k+1).mod(pk.p);\n");
    os.write("		u1 = pk.g1.modPow(r,pk.p);\n");
    os.write("		u2 = pk.g2.modPow(r,pk.p);\n");
    os.write("		e  = ((pk.h.modPow(r,pk.p)).multiply(m)).mod(pk.p);\n");
    os.write("		alpha = hashBitList(pk.k,pk.p,pk.hash,bitListThree(pk.k,u1,u2,e));\n");
    os.write("		v = ((pk.c.modPow(r,pk.p)).multiply(pk.d.modPow(r.multiply(alpha),pk.p))).mod(pk.p);\n");
    os.write("		cChunk = bitListFour(pk.k,u1,u2,e,v);\n");
    os.write("		\n");
    os.write("		for(i=0; i < 4*pk.k; i++)\n");
    os.write("		    res[resIndex++] = cChunk[i];\n");
    os.write("	    }\n");
    os.write("	    return res;\n");
    os.write("\n");
    os.write("	}\n");
    os.write("    \n");
    os.write("    /************************************************************\n");
    os.write("     * --  Decrypting  --\n");
    os.write("     ************************************************************/\n");
    os.write("\n");
    os.write("    public boolean[] decrypt(boolean[] cryptoText){\n");
    os.write("	boolean[] res = new boolean[((cryptoText.length / (4 * sk.k))+1)*(sk.k-1)];\n");
    os.write("	// one bit (the most significant one ) is always lost, since it is 0.\n");
    os.write("	\n");
    os.write("	boolean[] cChunk = new boolean[sk.k];\n");
    os.write("	boolean[] mChunk = new boolean[sk.k];\n");
    os.write("	BigInteger c;\n");
    os.write("	int index = 0;\n");
    os.write("	int resIndex = 0;\n");
    os.write("	int len   = cryptoText.length;\n");
    os.write("	int i;\n");
    os.write("\n");
    os.write("	byte[] zero = {0};\n");
    os.write("	BigInteger zeroBig = new BigInteger(zero); // will be returned, if key is faulty\n");
    os.write("	BigInteger u1,u2,e,v,alpha,m;\n");
    os.write("\n");
    os.write("	while(index+(4*sk.k) < len){ // only decrypt complete blocks\n");
    os.write("	    // Copy --- with all bits.\n");
    os.write("	    for(i = 0; i< sk.k; i++)\n");
    os.write("		cChunk[i] = cryptoText[index++];\n");
    os.write("	    u1 = bits2BigInteger(cChunk);\n");
    os.write("	    for(i = 0; i< sk.k; i++)\n");
    os.write("		cChunk[i] = cryptoText[index++];\n");
    os.write("	    u2 = bits2BigInteger(cChunk);\n");
    os.write("	    for(i = 0; i< sk.k; i++)\n");
    os.write("		cChunk[i] = cryptoText[index++];\n");
    os.write("	    e  = bits2BigInteger(cChunk);\n");
    os.write("	    for(i = 0; i< sk.k; i++)\n");
    os.write("		cChunk[i] = cryptoText[index++];\n");
    os.write("	    v = bits2BigInteger(cChunk);\n");
    os.write("	    \n");
    os.write("	    alpha = hashBitList(sk.k,sk.p,sk.hash,bitListThree(sk.k,u1,u2,e));\n");
    os.write("	    \n");
    //    os.write("	    if(u1.equals(BigInteger.ZERO)){ System.out.println(\"WARNING: u1 is zero!!!!\"); }\n");
    
    os.write("	    if((v.equals(((u1.modPow(sk.x1.add(alpha.multiply(sk.y1)),sk.p)                    ).multiply(u2.modPow(sk.x2.add(alpha.multiply(sk.y2)),sk.p))).mod(sk.p))))\n");
    os.write("		{\n");
    os.write("		    m = (e.multiply((u1.modPow(sk.z,sk.p)).modInverse(sk.p))).mod(sk.p);\n");
    os.write("		    cChunk = bigInteger2bits(sk.k, m);\n");
    os.write("		    for(i = 0; i < sk.k - 1; i++) // ignore most significant bit\n");
    os.write("			res[resIndex++] = cChunk[i];\n");
    os.write("		}\n");
    os.write("	    else\n");
    os.write("		for(i = 0; i < sk.k-1;  i++)\n");
    os.write("		    res[resIndex++] = true; // fill with ones\n");
    os.write("	}\n");
    os.write("		\n");
    os.write("	return res;\n");
    os.write("    }\n");
    os.write("}\n");
    os.write("\n");
    os.write("\n");
    os.write("/**\n");
    os.write(" * Auxillary data structure for hash function\n");
    os.write(" * \n");
    os.write(" */\n");

    os.write("class HashFunction {\n");
    os.write("    BigInteger hash_g1;\n");
    os.write("    BigInteger hash_g2;\n");
    os.write("\n");
    os.write("    HashFunction(byte[] hash_g1_rep,\n");
    os.write("		byte[] hash_g2_rep){\n");
    os.write("	hash_g1 = new BigInteger(hash_g1_rep);\n");
    os.write("	hash_g2 = new BigInteger(hash_g2_rep);\n");
    os.write("    }\n");
    os.write("}\n");
    os.write("\n");
    os.write("/**\n");
    os.write(" * Definition of the records structure of the keys<BR>\n");
    os.write(" *\n");
    os.write(" * The variable names used correspond to the ones in [CS98]\n");
    os.write(" *\n");
    os.write(" */\n");
    os.write("\n");
    os.write("\n");
    os.write("\n");
    os.write("\n");
    os.write("\n");
    os.write("\n");
    os.write("\n");
    os.write("class SecretKey {\n");
    os.write("    int          k;        // number of bits\n");
    os.write("    BigInteger   p;        // prime modulus\n");
    os.write("    BigInteger   x1;          \n");
    os.write("    BigInteger   x2;\n");
    os.write("    BigInteger   y1;\n");
    os.write("    BigInteger   y2;\n");
    os.write("    BigInteger   z;\n");
    os.write("    HashFunction hash;\n");
    os.write("    \n");
    os.write("    SecretKey(int k_id,\n");
    os.write("	      byte[] p_rep,\n");
    os.write("	      byte[] x1_rep,\n");
    os.write("	      byte[] x2_rep,\n");
    os.write("	      byte[] y1_rep,\n");
    os.write("	      byte[] y2_rep,\n");
    os.write("	      byte[] z_rep,\n");
    os.write("	      byte[] hash_g1_rep,\n");
    os.write("	      byte[] hash_g2_rep){\n");
    os.write("	k = k_id;\n");
    os.write("	p  = new BigInteger(p_rep);\n");
    os.write("	x1 = new BigInteger(x1_rep);\n");
    os.write("	x2 = new BigInteger(x2_rep);\n");
    os.write("	y1 = new BigInteger(y1_rep);\n");
    os.write("	y2 = new BigInteger(y2_rep);\n");
    os.write("	z  = new BigInteger(z_rep);\n");
    os.write("	hash = new HashFunction(hash_g1_rep,hash_g2_rep);\n");
    os.write("    }\n");
    os.write("}\n");
    os.write("\n");
    os.write("class PublicKey {\n");
    os.write("    int          k;\n");
    os.write("    BigInteger   p;\n");
    os.write("    BigInteger   g1;\n");
    os.write("    BigInteger   g2;\n");
    os.write("    BigInteger   c;\n");
    os.write("    BigInteger   d;\n");
    os.write("    BigInteger   h;\n");
    os.write("    HashFunction hash;\n");
    os.write("\n");
    os.write("    PublicKey(int k_id,\n");
    os.write("	      byte[] p_rep,\n");
    os.write("	      byte[] g1_rep,\n");
    os.write("	      byte[] g2_rep,\n");
    os.write("	      byte[] c_rep,\n");
    os.write("	      byte[] d_rep,\n");
    os.write("	      byte[] h_rep,\n");
    os.write("	      byte[] hash_g1_rep,\n");
    os.write("	      byte[] hash_g2_rep){\n");
    os.write("	k  = k_id;\n");
    os.write("	p  = new BigInteger(p_rep);\n");
    os.write("	g1 = new BigInteger(g1_rep);\n");
    os.write("	g2 = new BigInteger(g2_rep);\n");
    os.write("	c  = new BigInteger(c_rep);\n");
    os.write("	d  = new BigInteger(d_rep);\n");
    os.write("	h  = new BigInteger(h_rep);\n");
    os.write("	hash = new HashFunction(hash_g1_rep,hash_g2_rep);\n");
    os.write("    }\n");
    os.write("}\n");
    os.write("\n");
    os.write("\n");
    os.write("\n");
    os.write("\n");
    os.write("\n");
    os.write("\n");

    os.flush();
  }
}
