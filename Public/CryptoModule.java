/************************************************************
 * @(#)CryptoModule.java
 *
 * Requires: java 1.1.5 (or higher)
 * 
 *           [as e.g. provided in Netscape 4.06]
 *
 * References:
 * 
 * [CS98] Ronald Cramer and Victor Shoup: A practical public key crypto
 *        system provably secure against adaptive chosen  ciphertext attack
 *        in proceedings of Crypto 1998, LNCS 1462, p.13ff  
 * 
 * No Copyright (!c)
 */


import java.math.BigInteger;
import java.util.Random;

  /**
   * CryptoModule implements the Cramer-Shoup 
   * public key Crypto Algorithm
   *
   * If you want to use it, you have to modify this file and
   * add your own private key.
   * <PRE>
   * see: http://www.verify-it.de/sub/cramer_shoup.html
   *      http://www.verify-it.de/sub/crypter.html
   * </PRE>
   * 
   * @author <A HREF="MAILTO:omoeller@verify-it.de?subject=CryptoModule.java<2>%20(1.2%20Sun%20Jul%2014%2023:24:21%202002)">M. Oliver M&ouml;ller</A>
   * @version  $Revision: 1.2 $ 		$Date: 2004/07/07 06:52:54 $
   */
public class CryptoModule {

    /************************************************************
     * Public and secret keys
     ************************************************************/
    
  public String keyNameString = "Method:   Cramer-Shoup98\r\nKey-Name: Vanilla-2014\r\nOwner:    M. Oliver Moeller <omoeller@verify-it.de>\r\n";
  
  private SecretKey sk;
  private PublicKey pk;
  
  private Random rnd;
  
  public CryptoModule(){
    
    /****************************************
     * Initialize:
     * Set the keys to personal adjustings
     ****************************************/
    
    /*
     * Method:   Cramer-Shoup98
     * Key-Name: Vanilla-2014
     * Owner:    M. Oliver Moeller <omoeller@verify-it.de>
     */
    
    byte[] p = { 38,119,1,11,74,52,-119,123,-50,-85,-59,-86,42,-33,-89,-2,99,-57,-94,87,24,-38,6,103,-107,49,-61,-89,16,-67,-73,-117,40,93,20,98,-85,30,-127,109,51,-72,84,95,-74,-53,1,-120,-29,121,-49,48,29,103,98,22,-120,81,-107,4,-115,116,38,88,-108,-110,34,59,-67,8,118,9,32,-41,-59,-113,86,11,102,18,122,92,27,99,-40,-38,19,35,66,82,-74,-79,-50,-126,-101,-90,-122,-117,-42,109,-34,-88,-21,-49,-108,100,-90,48,17,88,-114,23,-119,8,78,119,-72,-47,68,-44,-31,-42,-13,-103,-42,124,-49,-19,-40,5,-95,123,-41,-34,86,39,-45,20,-46,46,104,110,-34,-62,-113,-86,19,-90,-98,-97,-108,-11,90,-65,39,102,96,-18,-122,-101,-111,-74,97,124,-73,119,-81,75,77,-18,43,-65,-9,-35,32,35,-18,76,1,58,-106,24,53,113,79,-61,66,-92,7,41,7,10,30,-74,83,-37,65,116,-32,107,61,9,118,31,-54,57,-62,57,-112,124,38,116,56,-76,33,79,-112,-25,-71,-76,24,114,107,100,-121,64,60,-79,110,18,-29,-68,35,46,-116,-110,22,115,-64,-48,53,53,-122,74,21,34,-95,94,-1,1,17,113 };
    byte[] hg1 = { 13,-51,-90,-27,40,-108,111,-84,86,23,35,-106,-49,94,103,-31,-123,102,-121,64,25,-89,-116,104,24,84,-33,-58,-116,20,0,10,56,13,-100,-13,-52,83,-78,-37,38,-88,62,-10,-21,-67,-18,-55,-18,-90,80,-2,-126,-1,11,-94,-100,50,-36,-78,119,-24,64,-43,-92,41,-55,69,-108,-1,100,-22,25,-10,-12,-96,-79,-104,112,-32,125,58,-62,-99,-73,-13,108,-62,-104,66,-111,-94,50,32,50,-101,-113,123,79,0,48,-109,46,-93,63,95,-119,-92,-49,-52,-31,14,79,-105,126,-17,82,-16,29,46,25,27,8,-27,105,-50,-18,-21,-105,-104,43,8,-74,118,-75,68,-113,-105,57,-59,83,105,2,33,84,127,67,68,-110,48,-86,-112,-106,21,50,-112,94,-115,-94,127,108,7,68,121,-121,-19,62,-70,33,-106,-44,-121,-11,74,-40,17,4,-121,55,80,-44,92,73,-121,-106,-31,125,36,-96,-112,-26,-111,-40,-110,86,-38,-68,-126,35,63,83,-47,-95,51,100,106,-22,58,-19,107,-105,42,56,-75,38,83,15,-31,57,80,84,-30,-95,44,80,41,-50,33,62,-45,-77,74,-21,-5,120,87,32,-77,-90,-68,-47,-119,-115,12,45,82,-35,-52,-45,-43,-26,-16 };
    byte[] hg2 = { 9,-56,43,75,-113,-55,-31,-119,108,-124,-127,34,124,86,-90,-38,-115,126,113,-34,106,83,22,48,22,-61,93,11,-90,77,-9,-82,95,-42,-121,-21,31,-41,13,26,-98,101,-8,117,-30,-124,109,6,-31,-117,102,-8,120,48,48,101,-111,-99,-18,110,67,87,18,-117,-85,36,-9,44,119,-90,108,-31,-80,-64,86,-112,68,-111,-108,-116,-28,-79,-111,-74,32,101,-46,29,-2,-26,15,-96,109,-84,-50,-75,70,-29,-36,-53,-81,113,-67,97,6,-102,41,-94,3,-46,15,51,-13,-14,-6,-124,103,-120,26,24,77,24,-97,-60,71,-86,0,-59,63,57,-47,-117,23,127,126,58,-106,47,44,31,-91,38,70,32,-44,72,-117,-89,4,-67,-27,-69,125,-1,-118,-115,-64,-64,47,-73,84,34,-55,124,-46,5,-96,87,-80,2,0,125,-70,90,40,-85,-26,-52,66,-104,-108,12,63,12,25,46,71,76,117,-118,-36,-29,76,117,110,66,127,7,85,26,-30,95,54,-68,49,-52,36,-31,-101,-70,-30,89,41,25,40,-44,-109,-20,72,38,-115,-50,17,60,-122,-75,48,-19,9,57,4,-44,14,-20,-18,-128,30,37,60,-104,-72,-6,-35,30,-81,81,-81,59,-35,-116,112,-128 };
    
    byte[] g1 = { 32,4,80,-54,16,104,-77,-3,-48,120,126,-98,-120,65,12,-73,-123,38,29,44,108,-96,-64,83,15,-120,122,-94,65,9,45,110,65,64,86,-68,39,95,114,65,-44,27,61,43,-128,-123,115,-5,46,32,-25,-122,-92,-82,66,39,-103,-107,40,28,-72,-116,28,8,21,29,-23,-100,-99,49,-42,89,120,-111,-38,-60,44,-14,56,71,-97,-3,-90,112,22,-24,68,44,-87,-64,1,53,5,-79,43,-124,31,-47,-15,40,43,-1,-98,-122,39,63,38,-81,70,-43,77,90,-96,125,125,-37,85,27,79,80,-88,-92,114,-5,99,14,-92,88,-81,59,72,-106,2,-46,-111,88,-13,85,32,-87,124,2,-100,90,53,46,-24,-50,0,-47,-57,-103,-118,41,-49,47,70,-56,86,101,-59,-70,-80,65,26,121,101,100,51,92,83,-107,10,12,-119,-47,-34,70,23,103,-57,-23,111,-109,101,86,39,45,106,69,53,-100,95,3,75,77,123,0,-69,25,39,38,-76,15,-96,-87,22,-67,30,-45,51,-106,-9,115,-103,39,10,-124,61,77,-82,-102,-88,-53,-115,20,33,70,66,-33,-7,110,122,107,-33,-92,1,0,-13,65,-69,16,33,9,27,14,92,-42,19,67,24,96 };
    byte[] g2 = { 12,-117,6,-80,78,111,20,30,-21,2,-120,-71,-84,-34,17,60,54,59,92,60,-27,92,43,-116,121,-20,-82,-26,98,-82,-61,-45,-36,124,-126,64,-67,25,-75,-90,61,-42,-93,119,-127,104,108,-30,67,-89,-73,-62,77,25,-21,-7,121,105,-27,103,-121,75,77,59,-56,108,-100,91,-40,23,62,12,106,103,101,-112,-4,-61,-102,126,-124,15,19,111,-21,108,-96,-55,-11,-7,94,108,-50,-106,123,30,23,-20,0,-126,81,26,20,35,-5,-46,-37,-3,37,40,-27,-20,50,-53,102,12,44,124,-122,114,-122,-47,-5,-10,57,-121,-69,102,-33,-107,-72,-108,-13,-21,-110,111,-36,59,-108,40,-39,-18,-71,-9,-94,50,8,-113,96,84,-104,-77,58,-58,-89,-56,-13,-63,-65,-11,124,-66,42,82,64,-108,127,-24,123,-26,-103,-121,91,-29,2,-98,127,123,-30,-99,59,-29,-29,113,70,67,-108,92,125,-6,81,127,110,7,-35,-29,-81,-82,88,61,72,85,-30,-17,-35,109,78,65,-42,62,74,114,88,-25,-58,13,40,-73,-112,90,99,78,-57,27,-69,-64,78,-30,70,-124,-24,80,70,19,-117,52,-8,85,10,-16,24,-92,119,-117,-90,60,42,-121,108,-93,123,-48 };
    
    
    byte[] c = { 33,-63,101,-124,60,66,-45,23,-32,-44,-80,69,-85,-108,-79,41,107,-7,6,-82,-87,6,-113,-5,9,-16,-69,53,113,83,-87,30,-47,67,61,-105,77,113,-51,119,-100,-81,-128,51,-21,-26,-30,21,-40,-78,-44,65,-50,-62,-56,27,-99,36,53,-74,47,-45,-47,-55,-13,-125,1,48,-12,6,-7,-17,94,-66,-38,84,87,-76,-32,51,-116,-35,3,-67,106,53,-122,29,-33,101,103,-58,-72,90,-71,39,-65,-128,61,-22,121,-48,40,51,125,14,10,-113,117,-39,-37,-15,58,90,98,14,115,-53,99,22,-25,29,32,88,71,-60,85,73,-119,-20,-107,36,108,81,-90,113,107,-30,-75,99,105,-1,-53,32,-106,36,116,66,-30,-127,46,67,-104,13,-116,74,-1,-53,102,-25,-15,28,76,-56,126,-55,46,4,-70,-55,74,47,-74,-53,77,97,58,56,30,-81,-106,92,-28,-110,62,-54,-77,45,-118,-64,30,-5,118,120,83,6,23,-123,71,-117,-88,-66,126,85,-10,42,-61,88,14,65,73,56,-116,69,8,104,15,-69,95,62,-48,-124,-20,122,48,-53,-38,71,-80,-100,94,42,36,4,-125,10,-11,57,-16,37,-116,-48,64,-50,-110,-25,-96,20,-56,-91,-81,-71 };
    byte[] d = { 37,-103,-94,124,-105,-32,41,19,63,-108,-21,4,-22,-118,23,-19,23,99,-63,41,-26,-6,124,62,6,124,122,-78,27,50,-126,-117,-62,-15,-94,-97,-119,4,7,-92,41,-49,101,7,-84,-2,15,54,-69,90,-36,-74,-107,-39,127,-69,-106,-101,-90,-126,44,-30,-58,-48,-108,-65,-76,14,82,84,-70,-43,-87,113,-73,81,-69,111,124,-107,-33,-109,14,92,-104,-29,57,81,-68,-32,31,-61,-16,117,89,30,95,31,8,124,54,-87,-76,-128,9,-100,67,-94,-55,26,79,-101,85,-2,97,50,52,-81,-107,-94,-5,92,-119,-120,-104,92,79,19,48,79,47,60,-82,-116,16,-50,-12,105,-98,57,-12,27,-78,74,-22,-70,-15,-116,-116,-64,116,112,111,-72,-52,73,41,-81,32,16,95,113,48,109,-40,-31,-41,119,-58,-121,-5,-123,-106,93,-110,52,-115,-6,-49,96,18,-65,95,59,-8,46,120,-80,-124,-17,104,2,64,-57,-119,-88,74,70,4,-31,-80,-61,-23,-85,34,10,27,-117,-108,-42,19,-50,121,107,75,-33,52,56,-23,-119,-111,-76,-41,59,86,-64,-7,10,-115,9,-49,17,96,100,-42,109,-107,69,-2,9,8,-43,2,71,-113,-79,78,56,-84,-59,88,-46 };
    byte[] h = { 6,77,-126,-59,73,-127,-53,-89,17,-95,39,-57,114,-31,-98,13,30,53,-80,-11,18,72,35,78,39,-103,99,-118,37,11,-99,107,108,-92,-66,-11,103,80,37,13,97,-40,-79,86,41,61,20,-126,-53,19,-109,102,-35,81,-118,-68,118,-126,-90,-60,-58,-50,-22,-95,-102,-92,46,-107,-25,-60,77,13,119,-51,-17,48,31,-24,61,-44,38,107,-6,-115,52,-105,-119,45,-64,-13,-35,21,20,-82,58,68,-126,-20,113,-127,51,-78,51,-10,-119,40,61,-119,-118,6,46,22,44,100,39,-72,10,19,65,11,-118,-96,43,108,-77,40,111,-75,-125,77,110,118,-78,-71,117,-52,-48,49,-44,-72,-39,33,-85,104,66,-74,-2,68,-32,18,47,60,-21,-80,-114,70,-5,30,67,-2,54,-63,-30,-22,-92,112,-105,-68,-89,-29,-120,9,80,40,-111,73,103,-117,-11,-8,-94,-48,123,-74,-97,-109,-114,-52,-46,-120,-26,-119,50,-73,-6,-126,6,-66,-2,50,-90,-29,-79,127,-45,-74,126,-70,-63,75,61,43,-15,62,82,-123,-41,-104,39,-23,-15,-36,91,20,-35,105,-88,-80,17,-19,32,1,118,123,-66,40,99,76,95,-98,-76,57,93,19,-96,-106,68,-24,120,-89,-52,5 };
    
    /*
     * Secret Key components start here
     */
    byte[] x1 = { 0 };
    byte[] x2 = { 0 };
    byte[] y1 = { 0 };
    byte[] y2 = { 0 };
    byte[] z =  { 0 };

    //****************************************
    
    pk = new PublicKey(2014,p,g1,g2,c,d,h,hg1,hg2);
    sk = new SecretKey(2014,p,x1,x2,y1,y2,z,hg1,hg2);
    
    /****************************************
     * A toy pair of valid secret and public keys...
     * if you copy this java source, you can use those to play around.
	 
	 byte[] p = { 8,15 };
	 byte[] g1 = { 2,-79 };
	 byte[] g2 = { 2,-111 };
	 byte[] c = { 3,22 };
	 byte[] d = { 2,-24 };
	 byte[] h = { 3,-90 };
	 byte[] hg1 = { 3,41 };
	 byte[] hg2 = { 1,-75 };
	 byte[] x1 = { 6,-45 };
	 byte[] x2 = { 7,-87 };
	 byte[] y1 = { 3,55 };
	 byte[] y2 = { 2,-127 };
	 byte[] z = { 1,-25 };
	 pk = new PublicKey(12,p,g1,g2,c,d,h,hg1,hg2);
	 sk = new SecretKey(12,p,x1,x2,y1,y2,z,hg1,hg2);

    ****************************************/
    
    
    /****************************************
     * Initiatize random generator
     ****************************************/
    
    rnd = new Random();
  }
  
  
  /************************************************************
   * Converter Methods
   ************************************************************/
  
  private BigInteger bits2BigInteger(boolean[] bits){
    int len = bits.length;
    byte[] bytes = new byte[2 + (len/8)];
    int b;
    int index = 1+ len/8;
    int bitIndex = 0;
    
    // in bits:  least significant bit comes first
    // in bytes: most significant bit is first!
    
    bytes[0] = 0;  // play safe - we do not want a negative number...
    bytes[1] = 0;
    
    while(bitIndex<len){
      b = 0;
      if((bitIndex<len)&&(bits[bitIndex++]))b = b|1;
      if((bitIndex<len)&&(bits[bitIndex++]))b = b|2;
      if((bitIndex<len)&&(bits[bitIndex++]))b = b|4;
      if((bitIndex<len)&&(bits[bitIndex++]))b = b|8;
      if((bitIndex<len)&&(bits[bitIndex++]))b = b|16;
      if((bitIndex<len)&&(bits[bitIndex++]))b = b|32;
      if((bitIndex<len)&&(bits[bitIndex++]))b = b|64;
      if((bitIndex<len)&&(bits[bitIndex++]))b = b|128;
      bytes[index--] = (byte)b;
    }
    return new BigInteger(bytes);
  }
  
  private boolean[] bigInteger2bits(int k, BigInteger b){
    boolean[] bits = new boolean[k];
    int i;
    
    for(i = 0; i< k; i++)
      bits[i] = b.testBit(i);
    return bits;
  }
  
  
  /************************************************************
   * Hashing
   ************************************************************/
  
  private BigInteger hashOnOneNumber(BigInteger p,
				     BigInteger g3,
				     BigInteger g4,
				     BigInteger nn){
    BigInteger q = p.shiftRight(1); // -1 div 2
    return hashOnTwoNumbers(p,g3,g4,nn.divide(q),nn.mod(q));
  }
  private BigInteger hashOnTwoNumbers(BigInteger p,
				      BigInteger g3,
				      BigInteger g4,
				      BigInteger a1,
				      BigInteger a2){
    return ((g3.modPow(a1,p)).multiply(g4.modPow(a2,p))).mod(p);
  }
  
  private BigInteger hashBitList(int k,
				 BigInteger p,
				 HashFunction hash,
				 boolean[] bits){
    BigInteger g3 = hash.hash_g1;
    BigInteger g4 = hash.hash_g2;
    
    int m   = k+k-2;
    int len = m - k - 1; // the length of ONE hash component
    int lengthDeficit = len - (bits.length % len);
    int index = - lengthDeficit; 
    int i;
    BigInteger arg;
    byte[] zero = {0};
    BigInteger res = new BigInteger(zero);
    BigInteger e;
    boolean[] transferList = new boolean[len];
    
    while(index < bits.length){
      // copy bits
      for(i = 0; i < len; i++){
	if(index < 0)
	  transferList[i] = false;
	else
	  transferList[i] = bits[index];
	index++;}
      
      arg = bits2BigInteger(transferList).add((res.shiftLeft(1)).setBit(0).shiftLeft(len));
      res = hashOnOneNumber(p, g3, g4, arg);
    }
    return res;
  }
  
  /************************************************************
   * Auxillary
   ************************************************************/
  
  private BigInteger bigRandom(int bits){
    // there should be a constructor with signature
    // BigInteger(int bits, Random rnd) that does that job....
    // only my compiler cannot find it for some reason.
    int nbytes = 2+bits/8;
    int i;
    int b;
    byte[] bytes = new byte[nbytes];
    
    for(i=1; i< nbytes; i++){
      b = (rnd.nextInt()) % 256;
      if(b>127)b = b - 256;
      bytes[i] = (byte)b;}
    bytes[0] = 1;
    
    return new BigInteger(bytes);
  };
  
  private boolean[] bitListOne(int k,
			       BigInteger b1){
    // LEAST significant bits first...
    boolean[] res = new boolean[k];
    int i;
    int index = 0;
    
    for(i = 0; i <k ; i++)
      res[index++] = b1.testBit(i);
    return res;
  }
  
  private boolean[] bitListThree(int k,
				 BigInteger b1,
				 BigInteger b2,
				 BigInteger b3){
    // LEAST significant bits first...
    boolean[] res = new boolean[3*k];
    int i;
    int index = 0;
    
    for(i = 0 ; i < k; i++)
      res[index++] = b1.testBit(i);
    for(i = 0 ; i < k; i++)
      res[index++] = b2.testBit(i);
    for(i = 0 ; i < k; i++)
      res[index++] = b3.testBit(i);
    
    return res;
  }
  private boolean[] bitListFour(int k,
				  BigInteger b1,
				  BigInteger b2,
				  BigInteger b3,
				  BigInteger b4){
	// least significant bits first...
	boolean[] res = new boolean[4*k];
	int i;
	int index = 0;

	for(i = 0 ; i < k; i++)
	    res[index++] = b1.testBit(i);
	for(i = 0 ; i < k; i++)
	    res[index++] = b2.testBit(i);
	for(i = 0 ; i < k; i++)
	    res[index++] = b3.testBit(i);
	for(i = 0 ; i < k; i++)
	    res[index++] = b4.testBit(i);

	return res;
    }

    /************************************************************
     * --  Encrypting  --
     ************************************************************/
    
	public boolean[] encrypt(boolean[] message){
	/***
	 * Returns a (big) array representing the
	 * concatenation of encryption blocks
	 *        [u1 u2 e v]
	 * Since the (bit-)length of each component of the block is given
	 * with k, it is not neccessary to encapsulate those additionally.
	 **************************************************/
	    
	    int messageLength = message.length;
	    int el            = pk.k - 1; // one 'bit' is not complete 
	    int i;
	    int toEncrypt     = (1+ (message.length / el))*pk.k*4;
	    boolean[] mChunk  = new boolean[pk.k];
	    mChunk[pk.k-1] = false;      // ignore most significant bit
	    //	    mChunk[0] = false;      // ignore most significant bit
	    boolean[] cChunk  = new boolean[pk.k*4];
	    boolean[] res     = new boolean[toEncrypt];
	    int resIndex      = 0;
	    
	    BigInteger r,u1,u2,e,m,alpha,v;

	    int pointer = 0;
	    int index = 0;


	    while(pointer < messageLength){
		index = 0;
		while((index < el)&&(pointer < messageLength))
		    mChunk[index++] = message[pointer++];
		while(index < el) // fill with random bits
		    mChunk[index++] = ( (rnd.nextInt() & 1) == 1);
		m  = bits2BigInteger(mChunk);

		r = bigRandom(pk.k+1).mod(pk.p);
		u1 = pk.g1.modPow(r,pk.p);
		u2 = pk.g2.modPow(r,pk.p);
		e  = ((pk.h.modPow(r,pk.p)).multiply(m)).mod(pk.p);
		alpha = hashBitList(pk.k,pk.p,pk.hash,bitListThree(pk.k,u1,u2,e));
		v = ((pk.c.modPow(r,pk.p)).multiply(pk.d.modPow(r.multiply(alpha),pk.p))).mod(pk.p);
		cChunk = bitListFour(pk.k,u1,u2,e,v);
		
		for(i=0; i < 4*pk.k; i++)
		    res[resIndex++] = cChunk[i];
	    }
	    return res;

	}
    
    /************************************************************
     * --  Decrypting  --
     ************************************************************/

    public boolean[] decrypt(boolean[] cryptoText){
	boolean[] res = new boolean[((cryptoText.length / (4 * sk.k))+1)*(sk.k-1)];
	// one bit (the most significant one ) is always lost, since it is 0.
	
	boolean[] cChunk = new boolean[sk.k];
	boolean[] mChunk = new boolean[sk.k];
	BigInteger c;
	int index = 0;
	int resIndex = 0;
	int len   = cryptoText.length;
	int i;

	byte[] zero = {0};
	BigInteger zeroBig = new BigInteger(zero); // will be returned, if key is faulty
	BigInteger u1,u2,e,v,alpha,m;

	while(index+(4*sk.k) < len){ // only decrypt complete blocks
	    // Copy --- with all bits.
	    for(i = 0; i< sk.k; i++)
		cChunk[i] = cryptoText[index++];
	    u1 = bits2BigInteger(cChunk);
	    for(i = 0; i< sk.k; i++)
		cChunk[i] = cryptoText[index++];
	    u2 = bits2BigInteger(cChunk);
	    for(i = 0; i< sk.k; i++)
		cChunk[i] = cryptoText[index++];
	    e  = bits2BigInteger(cChunk);
	    for(i = 0; i< sk.k; i++)
		cChunk[i] = cryptoText[index++];
	    v = bits2BigInteger(cChunk);
	    
	    alpha = hashBitList(sk.k,sk.p,sk.hash,bitListThree(sk.k,u1,u2,e));
	    
	    if((v.equals(((u1.modPow(sk.x1.add(alpha.multiply(sk.y1)),sk.p)                    ).multiply(u2.modPow(sk.x2.add(alpha.multiply(sk.y2)),sk.p))).mod(sk.p))))
		{
		    m = (e.multiply((u1.modPow(sk.z,sk.p)).modInverse(sk.p))).mod(sk.p);
		    cChunk = bigInteger2bits(sk.k, m);
		    for(i = 0; i < sk.k - 1; i++) // ignore most significant bit
			res[resIndex++] = cChunk[i];
		}
	    else
		for(i = 0; i < sk.k-1;  i++)
		    res[resIndex++] = true; // fill with ones
	}
		
	return res;
    }
}


/**
 * Auxillary data structure for hash function
 * 
 * @version  $Revision: 1.2 $ 		$Date: 2004/07/07 06:52:54 $
 */
class HashFunction {
    BigInteger hash_g1;
    BigInteger hash_g2;

    HashFunction(byte[] hash_g1_rep,
		byte[] hash_g2_rep){
	hash_g1 = new BigInteger(hash_g1_rep);
	hash_g2 = new BigInteger(hash_g2_rep);
    }
}

/**
 * Definition of the records structure of the keys<BR>
 *
 * The variable names used correspond to the ones in [CS98]
 *
 * @version  $Revision: 1.2 $ 		$Date: 2004/07/07 06:52:54 $
 */
class SecretKey {
    int          k;        // number of bits
    BigInteger   p;        // prime modulus
    BigInteger   x1;          
    BigInteger   x2;
    BigInteger   y1;
    BigInteger   y2;
    BigInteger   z;
    HashFunction hash;
    
    SecretKey(int k_id,
	      byte[] p_rep,
	      byte[] x1_rep,
	      byte[] x2_rep,
	      byte[] y1_rep,
	      byte[] y2_rep,
	      byte[] z_rep,
	      byte[] hash_g1_rep,
	      byte[] hash_g2_rep){
	k = k_id;
	p  = new BigInteger(p_rep);
	x1 = new BigInteger(x1_rep);
	x2 = new BigInteger(x2_rep);
	y1 = new BigInteger(y1_rep);
	y2 = new BigInteger(y2_rep);
	z  = new BigInteger(z_rep);
	hash = new HashFunction(hash_g1_rep,hash_g2_rep);
    }
}

class PublicKey {
    int          k;
    BigInteger   p;
    BigInteger   g1;
    BigInteger   g2;
    BigInteger   c;
    BigInteger   d;
    BigInteger   h;
    HashFunction hash;

    PublicKey(int k_id,
	      byte[] p_rep,
	      byte[] g1_rep,
	      byte[] g2_rep,
	      byte[] c_rep,
	      byte[] d_rep,
	      byte[] h_rep,
	      byte[] hash_g1_rep,
	      byte[] hash_g2_rep){
	k  = k_id;
	p  = new BigInteger(p_rep);
	g1 = new BigInteger(g1_rep);
	g2 = new BigInteger(g2_rep);
	c  = new BigInteger(c_rep);
	d  = new BigInteger(d_rep);
	h  = new BigInteger(h_rep);
	hash = new HashFunction(hash_g1_rep,hash_g2_rep);
    }
}

/**********************************************************************
 * Changelog:
 * $Log: CryptoModule.java,v $
 * Revision 1.2  2004/07/07 06:52:54  oli
 * removed Java 1.4 dependency again
 *
 *
 * 1.1 : added randomized fill beyond bounds of message
 *       (was: empty message built exception)
 *
 * 1.2 : fixed the random filling for the rest of the message
 * 
 **********************************************************************/
