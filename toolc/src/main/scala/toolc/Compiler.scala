package toolc

import scala.io.Source

import parser.Parser

class Compiler(val fileName: String) extends Reporter with Parser {
  import lexer.Tokens._

  val source: Source = Source.fromFile(fileName).withPositioning(true)

  def compile: Unit = {
    import parser.Trees._

    // Parsing
    var parsedTree: Option[Tree] = None
    parsedTree = Some(parseSource)
    terminateIfErrors

    val mainProg: Program = parsedTree match {
      case Some(p: Program) => p
      case _ => sys.error("Main program expected from parser.")
    }

    // pretty printing:
    println(TreePrinter(mainProg))
  }
}
