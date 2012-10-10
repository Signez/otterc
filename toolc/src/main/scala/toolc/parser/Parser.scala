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
    // ...
  }

  /** ''Eats'' the expected token, or terminates with an error. */
  private def eat(tokenClass: TokenClass): Unit = {
    // ...
  }

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenClass */
  private def expected(tokenClass: TokenClass, more: TokenClass*): Nothing = {
    fatalError("expected: " + (tokenClass::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
  }

  private def parseGoal: Tree = {
    // ...
    null
  }

  // ...

  /* One example: */
  private def parseIdentifier: Identifier = currentToken.info match {
    case ID(value) => { val ret = Identifier(value).setPos(currentToken); readToken; ret }
    case _ => expected(IDCLASS)
  }
}
