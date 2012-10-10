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
  
  def parseStatement : StatTree = {
    
    currentToken.tokenClass match {
    // println ( expression )
    case PRINTLN =>
	  readToken
      eat(OPAREN)
      val expr : ExprTree = parseExpression
      eat(CPAREN)
	  eat(SEMICOLON)
      return new PrintLn(expr)
    // | ident = expr
    case IDCLASS =>
      val ident : Identifier = parseIdentifier
      readToken
      eat(ASSIGN)
      val expr : ExprTree = parseExpression
      eat(SEMICOLON)
      return new Assignment(ident, expr)
      // | if ( expr ) statmt (else statmt)?
     case IF =>
      readToken
      eat(OPAREN)
      val expr : ExprTree = parseExpression
      eat(CPAREN)
      val stat : StatTree = parseStatement
      var elseStat : Option[StatTree] =
        if (currentToken == ELSE) {
          readToken
          new Some[StatTree](parseStatement)
        } else null
      return new If(expr, stat, elseStat)
      // | while ( expr ) statmt
     case WHILE =>
       readToken
       eat(OPAREN)
       val expr : ExprTree = parseExpression
       eat(CPAREN)
       val stat : StatTree = parseStatement
       return new While(expr, stat)
    }
  }
  
  def parseExpression : ExprTree = {}

  private def parseGoal: Tree = {
    null
  }

  // ...

  /* One example: */
  private def parseIdentifier: Identifier = currentToken.info match {
    case ID(value) => { val ret = Identifier(value).setPos(currentToken); readToken; ret }
    case _ => expected(IDCLASS)
  }
}
