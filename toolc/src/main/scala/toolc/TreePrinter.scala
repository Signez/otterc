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
        val printCond : String = printTree(condition, 0)
        val printThen : String = printTree(then, level+1)
        val printElze : String = ""
        if (elze.isDefined) {
          "else " + printTree(elze.get, level+1)
        }
        return "If" + printCond
      case _ => ""
    }
    "  " * level
  }
}
