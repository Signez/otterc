package toolc

import scala.io.Source
import parser.Parser
import analyzer.Analyzer
import analyzer.TypeChecker
import code.CodeGenerator
import java.io.File

class Compiler(val fileName: String)
  extends Reporter
  with Parser
  with Analyzer
  with TypeChecker
  with CodeGenerator {

  import lexer.Tokens._

  val file = new File(fileName)
  val shortName = file.getName
  val source = Source.fromFile(file).withPositioning(true)

  def compile(classDir: String): Unit = {
    import parser.Trees._
    import analyzer.Symbols._


    val outputDir = classDir + "/"
    
    val checkDir: java.io.File = new java.io.File(outputDir)
    if(!checkDir.exists) checkDir.mkdir
    if(!checkDir.isDirectory) fatalError("Cannot find output directory " + outputDir)
    
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

    // Type checking
    typeCheck(mainProg, global)
    terminateIfErrors

    // create main class
    generateMainClassFile(shortName, global, mainProg.main, outputDir)
    
    // Create classes
    mainProg.classes foreach {
      ct => generateClassFile(shortName,global,ct,outputDir)
    }
  }
}
