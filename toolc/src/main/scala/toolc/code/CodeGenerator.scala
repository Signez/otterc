package toolc
package code

trait CodeGenerator {
  self: Reporter =>

  import parser.Trees._
  import analyzer.Symbols._
  import analyzer.Types._
  import cafebabe._

  // Bytecodes
  import AbstractByteCodes._
  import ByteCodes._

  /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
  def generateClassFile(srcFileName: String, gs: GlobalScope, ct: ClassDecl, dir: String): Unit = {
    // ...
  }
}
