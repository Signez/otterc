package toolc
package analyzer

trait Analyzer {
  self: Reporter =>

  import parser.Trees._
  import Symbols._

  def analyzeSymbols(prog: Program): GlobalScope = {
    val gs = collectSymbols(prog)
    terminateIfErrors
    setSymbols(prog, gs)
    gs
  }

  private def collectSymbols(prog: Program): GlobalScope = {
    null
  }

  private def setSymbols(prog: Program, gs: GlobalScope): Unit = {
    
  }
}
