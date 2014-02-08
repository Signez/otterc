package otterc
package parser

import otterc.analyzer.Symbols

/**
 * Trees used in syntaxic tree.
 *
 * Contained trees may be mixed with Symbolic trait to represent parts that
 * are associated with symbols (classes, methods, variables...).
 */
object Trees {

  import analyzer.Symbols._
  import analyzer.Types._

  sealed trait Tree extends Positional

  case class Program(main: MainObject, var classes: List[ClassDecl]) extends Tree

  case class MainObject(id: Identifier, var stat: StatementTree) extends Tree with Symbolic[ClassSymbol]

  case class ClassDecl(id: Identifier, extendz: Option[Identifier],
                       variables: List[VarDecl], methods: List[MethodDecl]) extends Tree with Symbolic[ClassSymbol]

  case class VarDecl(id: Identifier, var theType: TypeTree) extends Tree with Symbolic[VariableSymbol]

  case class MethodDecl(id: Identifier, var arguments: List[VarDecl], returnType: TypeTree,
                        variables: List[VarDecl], var statements: List[StatementTree], var returnExpr: Option[ExpressionTree])
    extends Tree with Symbolic[MethodSymbol]


  sealed trait TypeTree extends Tree

  /**
   * Integer type reference.
   *
   * Int
   */
  case class IntType() extends TypeTree

  /**
   * Boolean type reference.
   *
   * Bool
   */
  case class BoolType() extends TypeTree

  /**
   * String type reference.
   *
   * String
   */
  case class StringType() extends TypeTree

  /**
   * Integer array type reference.
   *
   * Int[]
   */
  case class IntArrayType() extends TypeTree

  /**
   * Unit type reference.
   *
   * Unit
   */
  case class UnitType() extends TypeTree

  sealed trait StatementTree extends Tree

  /**
   * Block of statements.
   * 
   * { statements; }
   *
   * @param statements Statements contained in the block.
   */  
  case class Block(statements: List[StatementTree]) extends StatementTree

  /**
   * Conditional (if).
   * 
   * if(condition) statement; else otherwise;
   * 
   * @param condition Expression that, if true, causes statements to be run.
   * @param statement Statement which will be run if condition are true.
   * @param otherwise Optional statement; will be run if condition are false.
   */
  case class If(condition: ExpressionTree, statement: StatementTree, otherwise: Option[StatementTree]) extends StatementTree

  /**
   * Loop (while).
   *
   * while(condition) statements;
   * 
   * @param condition Expression that will decide if statements will be run
   *                  again.
   * @param statement Statement that will be run while condition is true.
   */
  case class While(condition: ExpressionTree, statement: StatementTree) extends StatementTree

  /**
   * Printing instruction.
   *
   * println(expression);
   *
   * @param expression Will be printed on standard output.
   */
  case class PrintLn(expression: ExpressionTree) extends StatementTree

  case class UnitExpression(expression: ExpressionTree) extends StatementTree

  sealed trait ExpressionTree extends Tree with Typed

  /**
   * Assignment instruction.
   *
   * identifier = expression;
   *
   * @param identifier Target variable.
   * @param expression New content that will be assigned to the variable.
   */
  case class Assignment(identifier: Identifier, expression: ExpressionTree) extends ExpressionTree

  /**
   * Array assignment instruction.
   *
   * identifier[index] = expression;
   *
   * @param identifier Target array.
   * @param index Target index.
   * @param expression New content that will be assigned to the variable (at index).
   */
  case class IndexAssignment(identifier: Identifier, index: ExpressionTree, expression: ExpressionTree) extends ExpressionTree

  /**
   * Sum operation.
   *
   * leftExpression + rightExpression
   *
   * @param leftExpression Left expression.
   * @param rightExpression Right expression.
   */
  case class Plus(leftExpression: ExpressionTree, rightExpression: ExpressionTree) extends ExpressionTree

  /**
   * Subtraction operation.
   *
   * leftExpression - rightExpression
   *
   * @param leftExpression Left expression.
   * @param rightExpression Right expression.
   */
  case class Minus(leftExpression: ExpressionTree, rightExpression: ExpressionTree) extends ExpressionTree

  /**
   * Multiplication operation.
   *
   * leftExpression * rightExpression
   *
   * @param leftExpression Left expression.
   * @param rightExpression Right expression.
   */
  case class Multiply(leftExpression: ExpressionTree, rightExpression: ExpressionTree) extends ExpressionTree

  /**
   * Division operation.
   *
   * leftExpression / rightExpression
   *
   * @param leftExpression Left expression.
   * @param rightExpression Right expression.
   */
  case class Divide(leftExpression: ExpressionTree, rightExpression: ExpressionTree) extends ExpressionTree

  /**
   * OR boolean operation.
   *
   * leftExpression || rightExpression
   *
   * @param leftExpression Left expression.
   * @param rightExpression Right expression.
   */
  case class Or(leftExpression: ExpressionTree, rightExpression: ExpressionTree) extends ExpressionTree

  /**
   * AND boolean operation.
   *
   * leftExpression && rightExpression
   *
   * @param leftExpression Left expression.
   * @param rightExpression Right expression.
   */
  case class And(leftExpression: ExpressionTree, rightExpression: ExpressionTree) extends ExpressionTree

  /**
   * Equality comparison.
   *
   * leftExpression == rightExpression
   *
   * @param leftExpression Left expression.
   * @param rightExpression Right expression.
   */
  case class Equals(leftExpression: ExpressionTree, rightExpression: ExpressionTree) extends ExpressionTree

  /**
   * Lesser than comparison.
   *
   * leftExpression < rightExpression
   *
   * @param leftExpression Left expression.
   * @param rightExpression Right expression.
   */
  case class LesserThan(leftExpression: ExpressionTree, rightExpression: ExpressionTree) extends ExpressionTree

  /**
   * Index access expression.
   *
   * array[index]
   *
   * @param array Array expression.
   * @param index Index expression.
   */
  case class Index(array: ExpressionTree, index: ExpressionTree) extends ExpressionTree

  /**
   * Length expression.
   *
   * expression.index
   *
   * @param expression Expression whose length will be calculated.
   */
  case class Length(expression: ExpressionTree) extends ExpressionTree

  /**
   * NOT boolean operation.
   * 
   * !expression
   * 
   * @param expression Boolean expression.
   */
  case class Not(expression: ExpressionTree) extends ExpressionTree

  /**
   * Method call expression.
   *
   * expression.methodId(parameters) 
   * 
   * @param expression Expression that evaluate to the object on which
   *                   method will be called.
   * @param methodId Method identifier.
   * @param parameters List of parameter expressions.
   */
  case class MethodCall(expression: ExpressionTree, methodId: Identifier,
                        parameters: List[ExpressionTree]) extends ExpressionTree


  /**
   * Integer literal.
   *
   * value (42)
   *
   * @param value Integer value.
   */
  case class IntegerLiteral(value: Int) extends ExpressionTree

  /**
   * String literal.
   *
   * "value"
   *
   * @param value String value.
   */
  case class StringLiteral(value: String) extends ExpressionTree

  /**
   * Boolean literal.
   *
   * value (true or false)
   *
   * @param value Boolean value.
   */
  case class BooleanLiteral(value: Boolean) extends ExpressionTree

  /**
   * New array.
   *
   * new Int[length]
   *
   * @param length Expression that represents length.
   */
  case class NewArray(length: ExpressionTree) extends ExpressionTree

  /**
   * New object.
   *
   * new classId()
   *
   * @param classId Identifier that represent the class.
   */
  case class NewObject(classId: Identifier) extends ExpressionTree

  /**
   * Instance call (this).
   *
   * this
   */
  case class ThisObject() extends ExpressionTree with Symbolic[ClassSymbol]

  /**
   * Identifier.
   *
   * value
   *
   * @param name String identifier.
   */
  case class Identifier(name: String) extends TypeTree with ExpressionTree with Symbolic[Symbol] {
    self =>

    override def setType(tpe: Type): self.type =
      sys.error("Setting a type to an identifier is impossible; use symbols instead.")

    override def getType: Type = {
      this.getSymbol match {
        case ms: MethodSymbol =>
          ms.getType
        case vs: VariableSymbol =>
          vs.getType
        case _ =>
          sys.error("Trying to get type from an identifier that is not associated with a symbol.")
          TUntyped
      }
    }
  }

}
