package whilelang

/*
 * TreeSimplifier will go through the tree of Statements and replace
 * all for-loops by while-loops
 */
object TreeSimplifier {
  /*
   * Search for for-loops inside a block
   */
  def replaceForLoop(body: List[Statement]): List[Statement] = {
    for {
      statB <- body
      n = statB match {
        case For(init, expr, step, body) => new Block(List(init, new While(expr, new Block(List(this(body), step)) )));
        case _ => this(statB);
      }
    } yield n
  }
  
  /*
   * Scan inside blocks, if-else-statements and while-loops for for-loops
   */
  def apply(stat: Statement): Statement = {
    stat match {
      case IfThenElse(expr, then, elze) => new IfThenElse(expr, this(then), this(elze))
      case While(expr, body) => new While(expr, this(body))
      case Block(body) => new Block(replaceForLoop(body))
      case _ => stat;
    }
  }
}