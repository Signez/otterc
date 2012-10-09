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
     ":" -> Token(COLON),
     "," -> Token(COMMA),
     "." -> Token(DOT),
     "!" -> Token(BANG),
     "&&" -> Token(AND),
     "||" -> Token(OR),
     "==" -> Token(EQUALS),
     "<" -> Token(LESS),
     "+" -> Token(PLUS),
     "-" -> Token(MINUS),
     "*" -> Token(MUL),
     "/" -> Token(DIV),
     ".length" -> Token(LENGTH),
     "[" -> Token(OBRACKET),
     "]" -> Token(CBRACKET),
     "(" -> Token(OPAREN),
     ")" -> Token(CPAREN),
     "{" -> Token(OBLOCK),
     "}" -> Token(CBLOCK),
     "true" -> Token(TRUE),
     "false" -> Token(FALSE),
     "this" -> Token(THIS),
     "new" -> Token(NEW),
     "=" -> Token(ASSIGN),
     "println" -> Token(PRINTLN),
     "while" -> Token(WHILE),
     "if" -> Token(IF),
     "else" -> Token(ELSE),
     ";" -> Token(SEMICOLON),
     "def" -> Token(DEF),
     "var" -> Token(VAR),
     "class" -> Token(CLASS),
     "extends" -> Token(EXTENDS),
     "return" -> Token(RETURN)
  );
  
  var buffer : String;
  
  /**
   * recursively searches for a key word
   */
  def mapKeyword(leftKeywords: Map[String, Token], charPos: Int): Token = {
    var filteredKeyword : Map[String, Token] = null;
    var alternative : Token = null;
    var longKeyword : Token = null;
    
    buffer += source.ch;
    //filter tokens that correspond to the current character
    for (
    	filtered <- leftKeywords;
    	(keyword: String, token: Token) = filtered;
        if (keyword(charPos) == source.ch)
    ) {
    	if (keyword.length() == charPos+1) {
    	  alternative = token;
    	} else {
    	  filteredKeyword += filtered
    	}
    }
    
    //if some token were found, filter next character
    if (!filteredKeyword.isEmpty){
      if (source.hasNext) {
    	source.next();
      	longKeyword = mapKeyword(filteredKeyword, charPos+1);
      }
    }
    
    if (longKeyword != null) {
      return longKeyword;
    } else if (alternative != null) {
      return alternative;
    } else {
      return null;
    }
  }

  /** Works like an iterator, and returns the next token from the input stream. */
  def nextToken: Token = {
	val token : Token = mapKeyword(keywordMap, 0);
	if (token == null)
	  //Todo: To be replaces by Stan's function
	  return Token(BAD)
	else
	  return token
  }
}
