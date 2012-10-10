package toolc
package parser

import lexer.Lexer

import scala.io.Source

/** LL parser for the Tool grammar. */
trait Parser extends Lexer {
  self: Compiler =>

  import Trees._
  import lexer.Tokens._

  def parseSource: Tree = {
    readToken // initializes the parser by calling the method in the Lexer.
    val tree: Tree = parseGoal
    terminateIfErrors
    tree
  }

  /** Store the current token, as read from the lexer. */
  private var currentToken: Token = Token(BAD)

  def readToken: Unit = {
    /* update currentToken using nextToken in the Lexer. */
    currentToken = nextToken
  }

  /** 
   * ''Eats'' the expected token, or terminates with an error. 
   * 
   * Similar to skip in the course. 
   */
  private def eat(expectedTokenClass: TokenClass): Unit = {
    if(expectedTokenClass != currentToken) {
      expected(expectedTokenClass);
    }
    readToken;
  }

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenClass */
  private def expected(tokenClass: TokenClass, more: TokenClass*): Nothing = {
    fatalError("expected: " + (tokenClass::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
  }
  
  def parseStatement : Tree = {
    // println ( stringConst , ident )
	if (currentToken == PRINTLN) { 
	  readToken;
      eat(OPAREN); /*stringConst*/; eat(COMMA);
      /*IDCLASS*/; eat(CPAREN);
    // | ident = expr
    } else if (currentToken == IDCLASS) {
      readToken;
      eat(ASSIGN); parseExpression
      // | if ( expr ) statmt (else statmt)?
    } else if (currentToken == IF) {
      readToken;
      eat(OPAREN); parseExpression; eat(CPAREN); parseStatement;
      if (currentToken == ELSE) { readToken; parseStatement; }
      // | while ( expr ) statmt
    }
  }
  
  def parseExpression : Tree = {}

  private def parseGoal: Tree = {
    eat(OBJECT);
    return Tree
    null
  }

  // ...

  /* One example: */
  private def parseIdentifier: Identifier = currentToken.info match {
    case ID(value) => { val ret = Identifier(value).setPos(currentToken); readToken; ret }
    case _ => expected(IDCLASS)
  }
}
