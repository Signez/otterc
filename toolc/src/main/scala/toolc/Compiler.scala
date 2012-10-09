package toolc

import scala.io.Source

import lexer.Lexer

class Compiler(val fileName: String) extends Reporter with Lexer {
  import lexer.Tokens._

  val source: Source = Source.fromFile(fileName)

  def compile: Unit = {
    var t: Token = Token(BAD)
      do {
      t = nextToken
      print(t.info + "(" + t.posString + ") ")
    } while(t.info != EOF)

    terminateIfErrors
  }  
}
