// -*- mode: JDE; c-file-style: \"stroustrup\"; c-basic-offset: 2; -*-
/***********************************************************************
 * @(#)Crypter.java   
 *
 * Requires: java 1.1.5 (or higher)
 *
 * No Copyright (!c)
 *
 * Crypter is a wrapper applet, that provides the interface for doing
 * online en/de-cryption with your web-browser.
 *
 * It imports only one non-standard class:    CryptoModule.java
 * This has to provide the field
 * 
 *      public String keyNameString
 *
 * and the two methods:
 *
 *      public boolean[] encrypt(boolean[] message)
 *      public boolean[] decrypt(boolean[] cryptoText)
 *
 * So you can easily replace it with a crypto algorithm of your coice.
 * (At the moment, this is just hard wired.) 
 * This file just contains the wrapper. All cryptographic stuff is going
 * on in CryptoModule.java
 *
 * References:
 * 
 * [CS98] Ronald Cramer and Victor Shoup: A practical public key crypto
 *        system provably secure against adaptive chosen  ciphertext attack
 *        in proceedings of Crypto 1998, LNCS 1462, p.13ff  
 *
 */

import java.net.*;

import java.awt.*;
import java.awt.event.*;

import java.io.File;
import java.io.InputStreamReader;
import java.io.FileInputStream;
import java.io.OutputStream;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;


// ==== Here is the Cryptographic Algorithm:
// import CryptoModule;   
// (not necessary to import it, since its in the same directory)
// ========================================

/*************************************************************
 * Contains all global constants
 *************************************************************/
class CONST {
  final static String  version    = "2.0." + new String("$Revision: 1.10 $").substring(13).replace('$',' ');
  final static String  TERMINATOR = "\n*\n"; 

  public static boolean truncateAfterTERMINATOR;
  public static boolean verbose = false;
  public static int bitsPerByte = 8;
}

/************************************************************
 * This is the wrapper that allows to to communicate with the
 * en/de-cryption algorithm implemented in CryptoModule.java
 * 
 * It will call en/decryption 
 * 
 * @author <A HREF="MAILTO:omoeller@verify-it.de?subject=Crypter.java%20(1.6.2%20Fri%20Feb%207%2023:49:25%202003)">M. Oliver M&ouml;ller</A>
 * @begun    99/09/26
 * @version  $Revision: 1.10 $            $Date: 2004/07/07 06:52:54 $
 ************************************************************/

public class Crypter extends java.applet.Applet implements Runnable {
  private static Color lightGray = new Color(170,170,170);
  
  private static Listener listener;
  
  private static Base64Handler b64;

  private static Label topLabel;
  
  private CryptoModule cm;
  
  public String dummy;


  /**
   * Initialize the applet.
   */
  public void init() {
    
    b64 = new Base64Handler();
    cm  = new CryptoModule();
    
    resize(900,800);
    this.setLayout(new BorderLayout());
    setBackground(lightGray);
    
    listener = new Listener();
    //listener.show();
    
    topLabel = new Label("-- Crypter " + CONST.version + " - Key: " + 
			 extractKeyName(cm.keyNameString)
			 + " --",
			 Label.CENTER);
    
    add(topLabel, "North");
    add(listener,"Center");
  }
  /**
   * Receive an input
   */
  public void setMessageText(String s){
    listener.inputArea.setText(s);
  }
  /**
   * Return current cryto text
   */
  public String getCryptoText(){
    return listener.outputArea.getText();
  }
  /**
   * Return the crypto text
   * replace newlines with javascript equivalent
   */
  public String getCryptoTextEscaped(){
    return escapeNewlines(listener.outputArea.getText());
  }
  
  /// ////////////////////////////////////////
  /// Auxillary    
  /// ////////////////////////////////////////
  
  /**
   * Don't show the listener
   * (or, if already displayed, make invisible)
   */
  public void hideInterface(){
    listener.setVisible(false);
    this.setVisible(false);
  }
  /**
   * Parse String with Key Name
   */
  private String extractKeyName(String s){
    String res = s;
    while( (res.length() > 0) && !res.startsWith("Key-Name:"))
      res = res.substring(1);
    if( res.length() == 0 )
      res = "*unknown*";
    else {
      res = res.substring(9).trim();
      res = res.substring(0,res.indexOf('\n'));
    }
    return res;
  }
  /** 
   * replace NL by html equivalents
   */
  private static String escapeNewlines(String s){
    StringBuffer res = new StringBuffer();
    char c;
    for(int i = 0; i < s.length(); i++){
      c = s.charAt(i);
      if( c == '\n' )
	res.append(" \n\r");
      else
	res.append(c);
    }
    return res.toString();
  }
  /**
   * Start Encryption
   */
  public void callEncrypt(){
    listener.messageLabel.setText("Encryption started (please wait)");
    listener.outputArea.setText("");
    repaint();
    listener.outputArea.setText(cm.keyNameString +
				b64.bits2base64(cm.encrypt(b64.string2bits(listener.inputArea.getText() + CONST.TERMINATOR + "\n" ))));
    listener.messageLabel.setText("--- Encryption finished.");
  }
  
  /// ////////////////////////////////////////
  ///  React to Buttons
  /// ////////////////////////////////////////
  
  public boolean action(Event evt, Object what) {
    if(evt.target instanceof Button){
      String buttonName = (String)what;
      
      if( buttonName.equalsIgnoreCase("Clear Message")){
	listener.inputArea.setText("");
	listener.messageLabel.setText("Message Text Cleared.");
      }
      else if( buttonName.equalsIgnoreCase("Clear Crypto")){
	listener.outputArea.setText("");
	listener.messageLabel.setText("Cryto Text Cleared.");
      }
      else if( buttonName.equalsIgnoreCase("Encrypt")){
	callEncrypt();
      }
      else if (buttonName.equalsIgnoreCase("Decrypt")){
	listener.messageLabel.setText("Decryption started (please wait)");
	listener.inputArea.setText("");
	repaint();
	listener.inputArea.setText(// "Derived Message Text:\n\n" +
				   b64.bits2stringTruncate(cm.decrypt(b64.base642bits(listener.outputArea.getText())),
							   CONST.truncateAfterTERMINATOR));
	listener.messageLabel.setText("--- Decryption finished.");
      }
      else if (buttonName.equalsIgnoreCase("Email Cryptotext")){
	listener.messageLabel.setText("Email does not work yet. Please copy output Window and send it that way.");
      }
    }
    repaint();
    
    return true;
  }
  
  public void update(Graphics g) {
    paint(g);
  }
  public void run() {
    this.repaint();
  }
  public void stop() {
  }
  // -- COMMAND LINE EXECUTION ---------------------------------------------
  public static void main(String argv []){
    try {
      Base64Handler b64 = new Base64Handler();
      CryptoModule  cm  = new CryptoModule();
      if( (argv.length < 2) || (argv.length > 4))
	throw new Exception("ERROR: illegal number of arguments.");
      int argCount = 0;
      String command = argv[argCount++];

      if((argv[argCount]).startsWith("-")){
	int nBits = - (new Integer(argv[argCount])).intValue();
	if( (nBits < 7) || (nBits > 17)){
	  throw new Exception("ERROR: number of bits set to an illegal value (" + nBits + ").");
	}
	System.err.println("** using " + nBits + " bits per charakter.");
	CONST.bitsPerByte = nBits;
	argCount++;
      }

      String inFileName = argv[argCount++];
      String outFileName = "";
      String clearTextCharset;
      String inCharset;
      String outCharset;

      if(argv.length > argCount)
	outFileName= argv[argCount];

      switch(CONST.bitsPerByte){
      case 7: 
	clearTextCharset  = "US-ASCII";
	break;
      case 8:
	clearTextCharset = "ISO-8859-1";
	break;
      default:
	clearTextCharset = "UTF-16BE";
      }

      StringBuffer inString = new StringBuffer();
      String outString = "*";
      char c;
      char[] cs = new char[1];
      int i = 0;	

      InputStreamReader in ;
      OutputStream dos;
      OutputStreamWriter dosw;
      
      // -- determine int/out charsets -------------------------
      if(  command.equals("e") || command.equals("encrypt")) {
	
	inCharset  = clearTextCharset;
	outCharset = "US-ASCII";

      }
      else if(  command.equals("d") || command.equals("decrypt") ||
		command.equals("u") || command.equals("untruncated-decrypt") ){

	inCharset  = "US-ASCII";
	outCharset = clearTextCharset;

      }
      else throw new Exception("ERROR: Unknown command >>" + command + "<<");

      // -- read input -----------------------------------------
      in =  new InputStreamReader(new FileInputStream(new File(inFileName)),
				  inCharset);
      System.err.println("** Reading stream with encoding " + in.getEncoding());
      while(in.ready()){
	in.read(cs, 0, 1);
	inString.append(cs); 
      }
      
      // -- execute ---------------------------------------------
      if(  command.equals("e") || command.equals("encrypt") ){


	System.err.print("** Encrypting...");
	outString = 
	  cm.keyNameString +
	  b64.bits2base64(cm.encrypt(b64.string2bits(inString.toString() +  CONST.TERMINATOR + "\n" )));
      } 
      else if(  command.equals("d") || command.equals("decrypt") ){

	System.err.print("** Decrypting...");
	outString = 
	  b64.bits2stringTruncate(cm.decrypt(b64.base642bits(inString.toString())),
				  true);
      }
      else if(  command.equals("u") || command.equals("untruncated-decrypt") ){

	System.err.print("** Decrypting (without truncation)...");
	outString = 
	  b64.bits2stringTruncate(cm.decrypt(b64.base642bits(inString.toString())),
				  false);
      }
      else throw new Exception("ERROR: Unknown command >>" + command + "<<");

      System.err.println("done.");

      // -- output result ---------------------------------------

      if(outFileName.equals("")){
	dos = System.out;
      }
      else {
	dos = new FileOutputStream(new File(outFileName));
      }
      
      dosw = new OutputStreamWriter(dos, outCharset);

      System.err.println("** Writing stream with encoding " + dosw.getEncoding());
      dosw.write(outString);
      dosw.close();
      System.out.println();
    }
    catch (Exception e){
      e.printStackTrace();
      printUsage();}
  }
  // -- AUX for command line -----------------------------------------------
  private static void printUsage(){
    System.out.println("Crypter V " + CONST.version + "   <omoeller@verify-it.de>\n");
    System.out.println("USAGE:   java Crypter COMMAND [-NAT] INFILE [OUTFILE]\n");
    System.out.println("  COMMAND: one of e (encrypt) or d  (decrypt)");
    System.out.println("               or u (untruncated-decrypt)");
    System.out.println("  INFILE can contain any sort of data.");
    System.out.println("  If option -NAT is set with 7 <= NAT <= 17,");
    System.out.println("     a char has NAT bits (both for en/decryption).");
    System.out.println("  If no OUTFILE is specified, the output goes to <stdout>.");

    System.exit(0);
  }
  // -----------------------------------------------------------------------
}

class Listener extends Panel {
  
  WindowAdapter l;
  Button encryptButton,decryptButton,emailButton,clearButton,clearcButton;
  TextArea inputArea,outputArea;
  Panel controlPanel,inputPanel,outputPanel,upperPanel,lowerPanel;
  Label inputLabel,outputLabel,messageLabel;
  
  Listener(){
    // Constructor
    
    inputLabel = new Label("Message Text:");
    inputArea = new TextArea("");
    try {
      //inputArea.setColumns(65);
    } catch (Exception e) {};
    inputPanel = new Panel();
    inputPanel.setLayout(new BorderLayout());
    inputPanel.add(inputLabel, "North");
    inputPanel.add(inputArea, "Center");
    
    outputLabel = new Label("Crypto Text (as Base64):");
    outputArea = new TextArea("");
    try { 
      //outputArea.setColumns(80);
    } catch (Exception e) {};
    outputPanel = new Panel();
    outputPanel.setLayout(new BorderLayout());
    outputPanel.add(outputLabel, "North");
    outputPanel.add(outputArea, "Center");
    
    clearButton   = new Button("Clear Message");
    clearcButton   = new Button("Clear Crypto");
    encryptButton = new Button("Encrypt");
    decryptButton = new Button("Decrypt");
    emailButton   = new Button("Email Cryptotext");
    messageLabel = new Label("No messages.");
    
    controlPanel = new Panel();
    controlPanel.setLayout(new GridLayout(5,1));
    controlPanel.add(clearButton);
    controlPanel.add(clearcButton);
    controlPanel.add(encryptButton);
    controlPanel.add(decryptButton);
    //!disabled	controlPanel.add(emailButton);
    
    
    upperPanel = new Panel();
    upperPanel.setLayout(new BorderLayout());
    upperPanel.add(inputPanel, "Center");
    upperPanel.add(controlPanel, "East");
    
    lowerPanel = new Panel();
    lowerPanel.setLayout(new BorderLayout());
    lowerPanel.add(outputPanel, "Center");
    lowerPanel.add(messageLabel, "South");
    
    this.setLayout(new GridLayout(2,1));
    this.add(upperPanel);
    this.add(lowerPanel);
  }
}

/************************************************************
 * This Class collects all the bitty details with converting
 * back and forth bit arraws, Strings and base64 encodings.
 *
 * There is nothing magical to it - it is just a pain in the
 * neck, so let's some object handle that.
 ************************************************************/
class Base64Handler {
  
  private static int linelength = 76; // lenght of encoding lines
  private static byte[] char2bits;
  private static char[] bits2char = {	'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/'};
  
  public Base64Handler(){
    
    char2bits = new byte[256];
    char2bits[65] = 0;
    char2bits[66] = 1;
    char2bits[67] = 2;
    char2bits[68] = 3;
    char2bits[69] = 4;
    char2bits[70] = 5;
    char2bits[71] = 6;
    char2bits[72] = 7;
    char2bits[73] = 8;
    char2bits[74] = 9;
    char2bits[75] = 10;
    char2bits[76] = 11;
    char2bits[77] = 12;
    char2bits[78] = 13;
    char2bits[79] = 14;
    char2bits[80] = 15;
    char2bits[81] = 16;
    char2bits[82] = 17;
    char2bits[83] = 18;
    char2bits[84] = 19;
    char2bits[85] = 20;
    char2bits[86] = 21;
    char2bits[87] = 22;
    char2bits[88] = 23;
    char2bits[89] = 24;
    char2bits[90] = 25;
    char2bits[97] = 26;
    char2bits[98] = 27;
    char2bits[99] = 28;
    char2bits[100] = 29;
    char2bits[101] = 30;
    char2bits[102] = 31;
    char2bits[103] = 32;
    char2bits[104] = 33;
    char2bits[105] = 34;
    char2bits[106] = 35;
    char2bits[107] = 36;
    char2bits[108] = 37;
    char2bits[109] = 38;
    char2bits[110] = 39;
    char2bits[111] = 40;
    char2bits[112] = 41;
    char2bits[113] = 42;
    char2bits[114] = 43;
    char2bits[115] = 44;
    char2bits[116] = 45;
    char2bits[117] = 46;
    char2bits[118] = 47;
    char2bits[119] = 48;
    char2bits[120] = 49;
    char2bits[121] = 50;
    char2bits[122] = 51;
    char2bits[48] = 52;
    char2bits[49] = 53;
    char2bits[50] = 54;
    char2bits[51] = 55;
    char2bits[52] = 56;
    char2bits[53] = 57;
    char2bits[54] = 58;
    char2bits[55] = 59;
    char2bits[56] = 60;
    char2bits[57] = 61;
    char2bits[43] = 62;
    char2bits[47] = 63;
  }
  public static boolean[] string2bits(String s){
    boolean[] bits = new boolean[s.length()*CONST.bitsPerByte];
    
    int  i;
    int  count = 0;
    char b;

    for(i = 0; i< s.length(); i++){
      b = (char)s.charAt(i);

      if(CONST.bitsPerByte > 17)
	bits[count++] = ((b & 131072) != 0);
      if(CONST.bitsPerByte > 16)
	bits[count++] = ((b & 65536) != 0);
      if(CONST.bitsPerByte > 15)
	bits[count++] = ((b & 32768) != 0);
      if(CONST.bitsPerByte > 14)
	bits[count++] = ((b & 16384) != 0);
      if(CONST.bitsPerByte > 13)
	bits[count++] = ((b & 8192) != 0);
      if(CONST.bitsPerByte > 12)
	bits[count++] = ((b & 4096) != 0);
      if(CONST.bitsPerByte > 11)
	bits[count++] = ((b & 2048) != 0);
      if(CONST.bitsPerByte > 10)
	bits[count++] = ((b & 1024) != 0);
      if(CONST.bitsPerByte > 9)
	bits[count++] = ((b & 512) != 0);
      if(CONST.bitsPerByte > 8)
	bits[count++] = ((b & 256) != 0);
      if(CONST.bitsPerByte > 7)
	bits[count++] = ((b & 128) != 0) ;

      bits[count++] = ((b &  64) != 0);
      bits[count++] = ((b &  32) != 0);
      bits[count++] = ((b &  16) != 0);
      bits[count++] = ((b &   8) != 0);
      bits[count++] = ((b &   4) != 0);
      bits[count++] = ((b &   2) != 0);
      bits[count++] = ((b &   1) != 0);}

    if(CONST.verbose){
      System.err.println("\n -- Input String length: " + s.length());
      System.err.println(" -- Input Bits: " + bits.length);
    }
    
    return bits;
  }
  
  

  public static String bits2string(boolean[] bits){
    return bits2stringTruncate(bits, false);
  }
  
  public static String bits2stringTruncate(boolean[] bits, boolean truncate){
    int i;
    StringBuffer result = new StringBuffer();
    String resString;
    int b = 0;
    char[] dummy = new char[1];
    char[] chars = new char[2+bits.length/(CONST.bitsPerByte)];
    int j = 0;	
    int counter = 0;

    if(CONST.verbose)
      System.err.println("\n -- Output Bits: " + bits.length);

    for(i = 0; i < bits.length; i++){

      b = (2*b);
      if(bits[i])b = (int)(b|1);
      counter++;
      if(counter == CONST.bitsPerByte){
	dummy[0] = (char)b;
	//	result.append(new String(dummy));
	chars[j++] = (char)b;
	counter = 0;
	b = 0;}}
    if(counter > 0){// do not ignore leftover bits
      b = (int)(((byte)b)<<(CONST.bitsPerByte-counter));
	chars[j++] = (char)b;
	//	dummy[0] = (char)b;
	//	result.append(new String(dummy));
    }

    while(j < chars.length)
	chars[j++] = '\n';	

    result = new StringBuffer(new String(chars));	

    resString = result.toString();

    if(truncate){
      for(i = result.length()-1; i >= 0; i--){
	if(resString.regionMatches(true, i,CONST.TERMINATOR, 0, CONST.TERMINATOR.length())){
	  if(CONST.verbose)
	    System.err.println(" !  Truncate");	
	  resString = result.substring(0,i);
	  i = -1;
	}
      }
    }
    if(CONST.verbose)
      System.err.println(" -- Output String length: " + resString.length());

    
    return resString;
  }
  
  public static boolean[] base64Core2bits(String s){
    boolean[] bits = new boolean[s.length()*6];
    
    int i;
    int count = 0;
    byte b;
    for(i = 0; i< s.length(); i++){
      b = char2bits[(int)s.charAt(i)];
      bits[count++] = ((b &  32) != 0);
      bits[count++] = ((b &  16) != 0);
      bits[count++] = ((b &   8) != 0);
      bits[count++] = ((b &   4) != 0);
      bits[count++] = ((b &   2) != 0);
      bits[count++] = ((b &   1) != 0);}
    
    return bits;
  }
  
  public static String string2base64(String s){
    return bits2base64(string2bits(s));
  }
  
  public static String bits2base64(boolean[] bits){
    int counter = 0;
    int linepos = 0;
    int i;
    int b = 0;
    
    StringBuffer result = 
      new StringBuffer("Crypter Version: " + CONST.version + "\n" +
		       "Bits per Char  : " + CONST.bitsPerByte + "\n" +
		       "begin-base64 CryptoText Input\r\n");
    for(i = 0; i < bits.length; i++){
      b = 2*b;
      if(bits[i])b = (b|1);
      counter++;
      if(counter == 6){
	result.append(bits2char[b]);
	linepos++;
	if(linepos == linelength){
	  result.append("\n");
	  linepos = 0;}
	counter = 0;
	b = 0;}}
    if(counter > 0){ // take care of leftover bits
      b = b<<(6-counter);     // shift it up front...
      result.append(bits2char[b]);}
    
    result.append("=\n==--==\n");
    
    return result.toString();
  }
  
  public boolean[] base642bits(String bs){
    StringBuffer base64string = new StringBuffer();
    boolean errorField[] = new boolean[0];
    int i;
    int offset = 0;
    int terminator = 0;
    char c;
    int len = bs.length();
    
    while((offset < len) &&
	  !bs.regionMatches(true,offset,"begin-base64",0,12))offset++;
    offset = bs.indexOf('\n',offset) + 1;
    terminator = bs.indexOf('=',offset);
    

    if((offset <= 0) || (terminator == -1))
      return errorField;
    
    for(i=offset; i < terminator; i++){
      c = bs.charAt(i);
      if((c != '\n') && (c != ' ')&&(c != '\t')&&(c != '\r'))
	base64string.append(c);}
    
    return base64Core2bits(base64string.toString());
  }
}

/**********************************************************************
 * Changelog
 *
 * $Log: Crypter.java,v $
 * Revision 1.10  2004/07/07 06:52:54  oli
 * removed Java 1.4 dependency again
 *
 * Revision 1.9  2004/07/07 06:16:28  oli
 * removed surplus Charset dependency
 *
 * Revision 1.8  2004/07/06 23:17:17  oli
 * changed: use java.nio (Java 1.4.2 or higher) to ensure
 * 	 proper clear text file encoding
 *
 * Revision 1.7  2003/02/07 22:52:25  oli
 * cosmetic byte -> char
 *
 * Revision 1.6  2003/02/07 22:50:27  oli
 * removed import of CryptoModule (fails for newer Java versions)
 *
 * Revision 1.5  2003/02/07 07:24:50  oli
 * included automatic truncation after TERMINATOR string
 *
 * Revision 1.4  2003/02/06 22:25:41  oli
 * generalizes n-bits option; changed TERMINATOR string
 *
 * -- Log of changes from older versions --
 *
 * 1.5 : updated to verify-it.default
 * 1.4 : added main() method for command-line exectution
 *       shortened default encoding line length
 *       made encoding apt for full 8 bits (binary files)
 *       ignore whitespaces in Base64 parts
 * 1.3 : revoked setColumns (older Netscapes cannot handle)
 * 1.2 : made accessible for javascipt
 * 1.1 : added terminator Characters 
 *
 **********************************************************************/
