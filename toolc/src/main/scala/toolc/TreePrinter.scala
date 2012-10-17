package toolc

object TreePrinter {
  import parser.Trees._

  def apply(t: Tree): String = {
    /* construct and return the appropriate string ... */
    printTree(t, 0)
  }
  
  def printTree(t: Tree, level: Int) : String = {
    val code: String =
    t match {
      case Program(main, classes) =>
        val printMain : String = printTree(main, 0)
        val printClasses : String =
          (for {cls : ClassDecl <- classes} yield printTree(cls, 0)).mkString("/n")
        return printMain + "/n" + printClasses
      case If(condition, then, elze) =>
        "If"
      case _ => ""
    }
    "  " * level
  }
}
