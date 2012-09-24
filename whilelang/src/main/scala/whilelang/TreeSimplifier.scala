package whilelang

/*object TreeSimplifier {
  def apply(stat: Statement): Statement = {
    // Replace all for loops by equivalent trees using while loops...
    println("Tree simplification is not implemented.")
    stat
  }
}*/

object TreeSimplifier {
  def replaceForLoop(body: List[Statement]): List[Statement] = {
    for {
      statB <- body
      n = statB match {
        case For(init, expr, step, body) => new Block(List(init, new While(expr, new Block(List(this(body), step)) )));
        case _ => this(statB);
      }
    } yield n
  }
  
  def apply(stat: Statement): Statement = {
    stat match {
      case IfThenElse(expr, then, elze) => new IfThenElse(expr, this(then), this(elze))
      case While(expr, body) => new While(expr, this(body))
      case Block(body) => new Block(replaceForLoop(body))
      case _ => stat;
    }
  }
}