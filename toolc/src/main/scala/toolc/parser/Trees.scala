package toolc
package parser

object Trees {
  import analyzer.Symbols._

  /*
   * Mix the Symbolic trait within the classes representing program parts that
   * require symbols, such as classes, variable declarations, identifiers,
   * etc..  The type parameter of the Symbolic Trait denotes the kind of symbol
   * to attach
   *
   * Examples:
   *    case class ClassDecl(id: Identifier, parent: Option[Identifier], vars: List[VarDecl], methods: List[MethodDecl]) extends Tree with Symbolic[ClassSymbol]
   *
   *    case class Identifier(value: String) extends TypeTree with ExprTree with Symbolic[Symbol]
   *
   *    case class This extends ExprTree with Symbolic[ClassSymbol]
   */

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
