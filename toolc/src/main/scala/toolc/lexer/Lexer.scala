package toolc
package lexer

import scala.io.Source

trait Lexer {
  // This trait will be composed with a Compiler. 
  // Implies that the methods from the Reporter class will be available at run-time, 
  // as well as the reference to the Source object.
  self: Compiler =>

  import Tokens._
  
  /**
   * Contains all keywords, i.e. special identifiers that are reserved
   * and used as part of the language.
   */
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
     "return" -> Token(RETURN),
     "object" -> Token(OBJECT)
  );
  
  /**
   * Read an identifier from the source, consuming the stream until neither 
   * a letter nor a digit shows up. 
   */
  def readIdentifier(): String = {
    var buffer = "";
    while(source.ch.isLetterOrDigit || source.ch == '_') {
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
   * Read the stream to find a new token.
   */
  def readToken(): Token = {
    var token = Token(BAD);
    
    if(source.pos == 0 && source.hasNext) source.next();
    
    // WHITESPACES ========================================
    // Trimming whitespaces
    while(source.ch.isWhitespace && source.hasNext) source.next();
    
    // COMMENTS ===========================================
    // Trimming comments, or returning DIV
    if(source.ch == '/') {
	    source.next()
	  
	    if(source.ch == '/') {
	    	// Trimming content of the comments until the new line
		    while(source.hasNext && source.ch != '\n') 
		      source.next();
		    
		    return readToken();
        } else {
        	return Token(DIV);
        }
    }
    
    // EOF ================================================
    // If we are at end of file, let's stop right know 
    // the process
    if(!source.hasNext) {
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
      source.next();
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
        source.next();
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
          // Returning immediately, to not consume a character (readaheading)
          return Token(BAD);
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
        println("\n - Unexpected character : " + source.ch + " (" + source.ch.asDigit + ")");
        Token(BAD); // Unexpected character ? BAD
      }
    }
    
    if(source.hasNext)
    	source.next();
    
    return token
  }

  /** 
   * Works like an iterator, and returns the next token from the input stream. 
   */
  def nextToken: Token = {
    val previously: Int = source.pos;
    val token: Token = readToken();
    
    // Fixing position (first next)
    if(source.pos == 0) source.next()
    
    return token.setPos(previously);
  }
}
