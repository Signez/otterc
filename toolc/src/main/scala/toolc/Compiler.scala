package toolc

import scala.io.Source
import parser.Parser
import analyzer.Analyzer

class Compiler(val fileName: String) extends Reporter with Parser with Analyzer {
  import lexer.Tokens._

  val source: Source = Source.fromFile(fileName).withPositioning(true)

  def compile: Unit = {
    import parser.Trees._
    import analyzer.Symbols._


    // Parsing
    var parsedTree: Option[Tree] = None
    parsedTree = Some(parseSource)
    terminateIfErrors

    val mainProg: Program = parsedTree match {
      case Some(p: Program) => p
      case _ => sys.error("Main program expected from parser.")
    }

    // Name analysis
    val global: GlobalScope = analyzeSymbols(mainProg)
    terminateIfErrors

    // Pretty-printing with symbols
    println(TreePrinter.withSymbolIDs(mainProg))
  }
}
