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

  /** Works like an iterator, and returns the next token from the input stream. */
  def nextToken: Token = {
    // ... 
    Token(EOF)
  }
}
