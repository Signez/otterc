package toolc
package parser

object Trees {
  sealed trait Tree extends Positional

  case class Program(main: MainObject, classes: List[ClassDecl]) extends Tree
  case class MainObject(id: Identifier, stat: StatTree) extends Tree
  /* etc. */

  sealed trait TypeTree extends Tree

  case class IntType extends TypeTree
  /* etc. */

  sealed trait StatTree extends Tree

  case class Block(stats: List[StatTree]) extends StatTree
  /* etc. */

  sealed trait ExprTree extends Tree

  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree
  /* etc. */

  case class Identifier(value: String) extends TypeTree with ExprTree // special case :)
}
