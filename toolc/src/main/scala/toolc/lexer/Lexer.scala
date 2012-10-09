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
   * Consume all whitespaces characters from the stream, and stop
   * when a non-whitespace characters shows up.
   */
  def trimWhitespaces() : Unit = {
    while(source.ch.isWhitespace && source.hasNext) source.next();
  }
  
  /**
   * Read the stream to find a new token.
   */
  def readToken(): Token = {
    var pos = 1;
    var token = Token(BAD).setPos(1);
    
    if(source.pos == 0 && source.hasNext) source.next();
    
    // WHITESPACES ========================================
    // Trimming whitespaces
    trimWhitespaces();
    
    // EOF ================================================
    // If we are at end of file, let's stop right know 
    // the process
    if(!source.hasNext) {
      return Token(EOF).setPos(source.pos)
    }
    
    // IDENTIFIERS & KEYWORDS =============================
    // All identifiers and keywords starts with integers
    if(source.ch.isLetter) {
      pos = source.pos;
      var sourceString = readIdentifier();
      // Get the corresponding keyword-token if it exists, else return a simple identifier
      return keywordMap.get(sourceString).getOrElse[Token](Token(ID(sourceString))).setPos(pos);
    }
    
    // STRINGS LITTERALS ==================================
    if(source.ch == '"') {
      pos = source.pos;
      source.next();
      var text = readEverythingUntil('"');
      if(source.ch == '"') {
        source.next();
        return Token(STRINGLITERAL(text)).setPos(pos);
      } else {
        return Token(BAD).setPos(pos);
      }
    }
    
    // INTEGER LITTERALS ==================================
    if(source.ch >= '0' && source.ch <= '9') {
      if(source.ch == '0') {
        source.next();
        return Token(INTEGERLITERAL(0)).setPos(source.pos);
      } else {
        pos = source.pos;
        var integer = readInteger();
        return Token(INTEGERLITERAL(integer)).setPos(pos);
      }
    }
    
    // OTHER (SIMPLE) TOKENS ==============================
    
    pos = source.pos;
    
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
          println("\n - Unexpected \"&\" characters, expecting \"&&\"");
          // Returning immediately, to not consume the following character (readaheading)
          return Token(BAD).setPos(source.pos);
        }
      }
      case '|' => {
        source.next()
        if(source.ch == '|') {
          Token(OR);
        } else {          
    	  println("\n - Unexpected \"|\" characters, expecting \"||\"");
    	  // Returning immediately, to not consume the following character (readaheading)
          return Token(BAD).setPos(source.pos);
        }
      }
      case '=' => {
        source.next()
        if(source.ch == '=') {
          // Two equals signs ?
          Token(EQUALS);
        } else {
          // Only on equals signs ?
          // Returning immediately, to not consume the following character (readaheading)
          return Token(ASSIGN).setPos(pos);
        }
      }
      case '/' => {
	    source.next()
	  
	    if(source.ch == '/' || source.ch == '*') {
	      	// Handling 
	    	if(source.ch == '/') {
		    	// Trimming content of the comments until the new line
			    while(source.hasNext && source.ch != '\n') 
			      source.next();	    	  
	    	} else {
	    	    // Trimming content of the comments until */
			    while(source.hasNext && !(source.ch == '*' && source.next() == '/')) 
			      source.next();
			    
			    if(!source.hasNext) {
			      println("\n - Unexpected EOF, expecting \"/*\"");
			      return Token(BAD).setPos(source.pos);
			    }
	    	}
		    
		    if(source.hasNext) source.next(); // Consume the \n or / (in */)
		    
		    readToken(); // Find another token
        } else {
        	return Token(DIV).setPos(source.pos - 1);
        }
	  }
      case _ => {
        println("\n - Unexpected character : " + source.ch + " (" + source.ch.asDigit + ")");
        Token(BAD); // Unexpected character ? BAD
      }
    }
    
    if(source.hasNext)
    	source.next();
    
    token.setPos(pos);
    
    return token;
  }

  /** 
   * Works like an iterator, and returns the next token from the input stream. 
   */
  def nextToken: Token = {
    // Fixing position (first next)
    if(source.pos == 0) source.next()
    
    return readToken();
  }
}
