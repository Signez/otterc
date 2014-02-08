package otterc
package analyzer

import otterc.parser.Trees.ExpressionTree
import java.lang.reflect.Constructor

trait Expander {
  self: Reporter =>

  import parser.Trees._
  import analyzer.Symbols._
  import analyzer.Types._

  def expand(prog: Program, gs: GlobalScope) {
    prog.classes = prog.classes.map(clazz => expand(clazz))

    prog.main.stat = expand(prog.main.stat)
  }

  def expand(clazz: ClassDecl): ClassDecl = {
    for (method <- clazz.methods) {
      method.statements = method.statements.map(stat => expand(stat))

      if (method.returnExpr.isDefined) {
        method.returnExpr = Some(expand(method.returnExpr.get))
      }
    }
    clazz
  }

  def expand(statement: StatementTree): StatementTree = {
    (statement match {
      case Block(statements) => 
        new Block(statements.map(stat => expand(stat)))
      case If(condition, stat, otherwise) =>
        new If(expand(condition), expand(stat), expand(otherwise))
      case PrintLn(expression) =>
        new PrintLn(expand(expression))
      case While(condition, expression) =>
        new While(condition, expand(expression))
      case UnitExpression(expression) =>
        new UnitExpression(expand(expression))
    }).setPosition(statement)
  }

  def callOverloadedOperator[T <: ExpressionTree : Manifest](lhs: ExpressionTree, rhs: ExpressionTree, methodName: String) : ExpressionTree = {
    lhs.getType match {
      case typ@TObject(classSymbol) =>
        // Method existence was checked at type checking
        new MethodCall(expand(lhs), Identifier(methodName).setSymbol(classSymbol.lookupMethod(methodName, List(rhs.getType)).get), List(expand(rhs)))
      case _ =>
        // These two cryptic lines simply does "new T(expand(lhs), expand(rhs))" using Scala reflect tools
        val treeToCreate = manifest[T].runtimeClass.getConstructor(classOf[ExpressionTree], classOf[ExpressionTree]).asInstanceOf[Constructor[T]]

        treeToCreate.newInstance(expand(lhs), expand(rhs)).asInstanceOf[T]
    }
  }

  def expand(expression: ExpressionTree): ExpressionTree = {
    val expanded : ExpressionTree = expression match {
      case id@Identifier(_) => id

      case Plus(lhs, rhs) => callOverloadedOperator[Plus](lhs, rhs, "plus")

      case Minus(lhs, rhs) => callOverloadedOperator[Minus](lhs, rhs, "minus")
      case Multiply(lhs, rhs) => callOverloadedOperator[Multiply](lhs, rhs, "multiply")
      case Divide(lhs, rhs) => callOverloadedOperator[Divide](lhs, rhs, "divide")
      case Or(lhs, rhs) => callOverloadedOperator[Or](lhs, rhs, "or")
      case And(lhs, rhs) => callOverloadedOperator[And](lhs, rhs, "and")
      case Equals(lhs, rhs) => new Equals(expand(lhs), expand(rhs))
      case LesserThan(lhs, rhs) => new LesserThan(expand(lhs), expand(rhs))
      case Index(lhs, rhs) => new Index(expand(lhs), expand(rhs))
      case Length(expr) => new Length(expand(expr))
      case Not(expr) => new Not(expand(expr))
      case MethodCall(objectId, methodId, expressions) =>
        new MethodCall(expand(objectId), methodId, expressions.map(expr => expand(expr)))

      case i@IntegerLiteral(value) => i
      case s@StringLiteral(value) => s
      case b@BooleanLiteral(value) => b

      case NewArray(length) => new NewArray(expand(length))
      case o@NewObject(objectId) => o

      case thisO@ThisObject() => thisO

      case assign@Assignment(id, expr) =>
        new Assignment(id, expand(expr))
      case IndexAssignment(identifier, index, expr) =>
        new IndexAssignment(identifier, expand(index), expand(expr))
    }

    val symbolized : ExpressionTree = expanded match {
      case id@Identifier(value) => {
        id.setSymbol(expression.asInstanceOf[Identifier].getSymbol)
      }
      case _ => expanded.setType(expression.getType)
    }

    symbolized.setPosition(expression)
  }

  def expand(option: Option[StatementTree]): Option[StatementTree] = option match {
    case Some(statement) => Some(expand(statement))
    case None => None
  }
}
