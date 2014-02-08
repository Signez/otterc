package otterc

import scala.io.Source
import parser.Parser
import otterc.analyzer.{Expander, Analyzer, TypeChecker}
import code.CodeGenerator
import java.io.File

/**
 * Compiles a source written in otter into JVM bytecode.
 *
 * This class uses Parser, Analyzer, TypeChecker, Expander
 * and CodeGenerator traits where actual compilation is done;
 * this class only defines a Compiler#compile method that
 * call method derived from these traits.
 *
 * @param fileName Source file name.
 */
class Compiler(val fileName: String)
  extends Reporter
  with Parser
  with Analyzer
  with TypeChecker
  with CodeGenerator
  with Expander {

  val file = new File(fileName)
  val shortName = file.getName
  val source = Source.fromFile(file).withPositioning(on = true)

  /**
   * Compile source code into bytecode.
   *
   * @param classDir Class directory where class files will be generated.
   */
  def compile(classDir: String): Unit = {
    import parser.Trees._
    import analyzer.Symbols._

    val outputDir = classDir + "/"

    val checkDir: java.io.File = new java.io.File(outputDir)
    if (!checkDir.exists) checkDir.mkdir
    if (!checkDir.isDirectory) fatalError("Cannot find output directory " + outputDir)

    // Parsing
    var parsedTree: Option[Tree] = None
    parsedTree = Some(parseSource)
    terminateIfErrors()

    val mainProg: Program = parsedTree match {
      case Some(p: Program) => p
      case _ => sys.error("Main program expected from parser.")
    }

    // Name analysis
    val global: GlobalScope = analyzeSymbols(mainProg)
    terminateIfErrors()

    // Type checking
    typeCheck(mainProg, global)
    terminateIfErrors()

    // Adding syntaxic sugar
    expand(mainProg, global)

    //print(TreePrinter(true)(mainProg))
    terminateIfErrors()

    // create main class
    generateMainClassFile(shortName, global, mainProg.main, outputDir)

    // Create classes
    mainProg.classes foreach {
      ct => generateClassFile(shortName, global, ct, outputDir)
    }
  }
}
