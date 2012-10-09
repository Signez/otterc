package toolc
package lexer

import scala.io.Source

trait Lexer {
  // This indicates to the Scala compiler that this trait will be composed with
  // a Compiler. This implies that the methods from the Reporter class will be
  // available at run-time, as well as the reference to the Source object.
  self: Compiler =>

  import Tokens._

  // You have access to the variable source defined in Compiler.scala, of the type
  // scala.io.Source in here. You need to use it as your character stream for the
  // file you are reading. You can use source.hasNext and source.next to read
  // characters, and source.pos to access the positions. Make sure all the tokens
  // you create have the proper position (you can always test that by calling an 
    // error on them and check the output).
  // 
  // Write as many helper functions as you need.
  
  val keywordMap = Map[String, Token](
     "if" -> Token(IF),
     "else" -> Token(ELSE),
     "extends" -> Token(EXTENDS),
     "length" -> Token(LENGTH),
     "true" -> Token(TRUE),
     "false" -> Token(FALSE),
     "this" -> Token(THIS),
     "new" -> Token(NEW),
     "println" -> Token(PRINTLN),
     "while" -> Token(WHILE),
     "def" -> Token(DEF),
     "var" -> Token(VAR),
     "class" -> Token(CLASS),
     "extends" -> Token(EXTENDS),
     "return" -> Token(RETURN)
  );
  
  /*def mapKeyword(leftKeyword: Map[String, Token], buffer: String): Token = {
    var currentBuffer = buffer + source.ch;
    for(keyword: String, token: Token <- leftKeyword if keyword.startsWith(currentBuffer)) {
      
    }
  }*/
  
  /**
   * Read an identifier from the source, consuming the stream until neither 
   * a letter nor a digit shows up. 
   */
  def readIdentifier(): String = {
    var buffer = "";
    while(source.ch.isLetterOrDigit) {
      buffer += source.ch;
      source.next();
    }
    return buffer;
  }
  
  /**
   * Read an integer from the source, consuming the stream until a non-digit 
   * character shows up.
   */
  def readInteger(): Int = {
    var buffer = "";
    while(source.ch.isDigit) {
      buffer += source.ch;
      source.next();
    }
    return buffer.toInt;
  }
  
  /**
   * Read any character until `endChar` shows up in the stream.
   */
  def readEverythingUntil(endChar : Char): String = {
    var buffer = "";
    while(source.ch != -1 && source.ch != endChar) {
      buffer += source.ch;
      source.next();
    }
    return buffer;
  }

  /** 
   * Works like an iterator, and returns the next token from the input stream. 
   */
  def nextToken: Token = {
    var token = Token(BAD);
    
    // WHITESPACES ========================================
    // Trimming whitespaces
    while(source.ch.isWhitespace) source.next();
    
    // COMMENTS ===========================================
    // Trimming comments
    if(source.ch == '/') {
	    source.next()
	  
	    if(source.ch == '/') {
		    while(source.ch != -1 && source.ch != '\n') 
		      source.next();
        }
    }
    
    // EOF ================================================
    if(source.ch == -1) {
      return Token(EOF)
    }
    
    // IDENTIFIERS & KEYWORDS =============================
    // All identifiers and keywords starts with integers
    if(source.ch.isLetter) {
      var sourceString = readIdentifier();
      // Get the corresponding keyword-token if it exists, else return a simple identifier
      return keywordMap.get(sourceString).getOrElse[Token](Token(ID(sourceString)));
    }
    
    // STRINGS LITTERALS ==================================
    if(source.ch == '"') {
      var text = readEverythingUntil('"');
      if(source.ch == '"') {
        source.next();
        return Token(STRINGLITERAL(text));
      } else {
        return Token(BAD);
      }
    }
    
    // INTEGER LITTERALS ==================================
    if(source.ch >= '0' && source.ch <= '9') {
      if(source.ch == '0') {
        return Token(INTEGERLITERAL(0));
      } else {
        var integer = readInteger();
        return Token(INTEGERLITERAL(integer));
      }
    }
    
    
    // OTHER (SIMPLE) TOKENS ==============================
    
    token = source.ch match {
      case '(' => Token(OPAREN)
      case ')' => Token(CPAREN)
      case '[' => Token(OBRACKET)
      case ']' => Token(CBRACKET)
      case '{' => Token(OBLOCK)
      case '}' => Token(CBLOCK)
      case '/' => Token(DIV)
      case '+' => Token(PLUS)
      case '-' => Token(MINUS)
      case '*' => Token(MUL)
      case ',' => Token(COMMA)
      case ':' => Token(COLON)
      case ';' => Token(SEMICOLON)
      case '.' => Token(DOT)
      case '!' => Token(BANG)
      case '<' => Token(LESS)
      case '&' => {
        source.next()
        if(source.ch == '&') {
          Token(AND);
        } else {
          // Returning immediately, to not consume a character (readaheading)
          return Token(BAD);
        }
      }
      case '|' => {
        source.next()
        if(source.ch == '|') {
          Token(OR);
        } else {
          Token(BAD);
        }
      }
      case '=' => {
        source.next()
        if(source.ch == '=') {
          Token(EQUALS);
        } else {
          // Returning immediately, to not consume a character (readaheading)
          return Token(ASSIGN);
        }
      }
      case _ => {
        Token(BAD); // Unexpected character ? BAD
      }
    }
    
    source.next();
    
    return token
  }
}
