package whilelang

sealed abstract class Tree

sealed abstract class Statement extends Tree
sealed abstract class SingleStatement extends Statement
case class Print(msg: String, varID: String) extends SingleStatement
case class Assign(varID: String, expr: Expression) extends SingleStatement
case object Skip extends SingleStatement
case class Block(body: List[Statement]) extends Statement
case class IfThenElse(expr: Expression, then: Statement, elze: Statement) extends Statement
case class While(expr: Expression, body: Statement) extends Statement
case class For(init: SingleStatement, expr: Expression, step: SingleStatement, body: Statement) extends Statement

sealed abstract class Expression extends Tree
case class Var(varID: String) extends Expression
case class IntLiteral(value: Int) extends Expression
case class Plus(lhs: Expression, rhs: Expression) extends Expression
case class Minus(lhs: Expression, rhs: Expression) extends Expression
case class Times(lhs: Expression, rhs: Expression) extends Expression
case class Division(lhs: Expression, rhs: Expression) extends Expression
case class Modulo(lhs: Expression, rhs: Expression) extends Expression
case class Equals(lhs: Expression, rhs: Expression) extends Expression
case class GreaterThan(lhs: Expression, rhs: Expression) extends Expression
case class LessThan(lhs: Expression, rhs: Expression) extends Expression
case class And(lhs: Expression, rhs: Expression) extends Expression
case class Or(lhs: Expression, rhs: Expression) extends Expression
case class Neg(expr: Expression) extends Expression
case class Not(expr: Expression) extends Expression

object TreePrinter {
  var level: Int = 0
  def incLevel = { level = level + 1 }
  def decLevel = { level = level - 1 }
  def printInc = { print((0 to level).map(l => "").mkString("  ")) }
  
  def apply(stat: Statement): Unit = {
    def printInline(s: Statement) = s match {
      case ss: SingleStatement => { printInc; this(ss); println(";"); }
      case _ => this(s)
    }
    
    stat match {
      case Print(msg, varID) => {
        print("println(\"" + msg + "\", " + varID + ")")
      }
      case Assign(varID, expr) => {
        print(varID + " = ")
        this(expr)
      }
      case IfThenElse(expr, then, elze) => {
        printInc; print("if ("); this(expr); println(") {")
        incLevel; printInline(then); decLevel
        
        elze match {
          case Skip => ;
          case _ => {
            printInc; println("} else {")
            incLevel; printInline(elze); decLevel
          }
        }
        printInc; println("}")
      }
      case While(expr, body) => {
        printInc; print("while ("); this(expr); println(") {")
        incLevel; printInline(body); decLevel
        printInc; println("}")
      }
      case For(init, expr, step, body) => {
        printInc
        print("for ("); this(init); print("; "); this(expr); print("; "); this(step); println(") {")
        incLevel; printInline(body); decLevel
        printInc; println("}")
      }
      case Block(body) => body.foreach(printInline(_))
      case Skip => ;
    }
  }
  
  def apply(expr: Expression): Unit = expr match {
    case Var(varID) => print(varID)
    case IntLiteral(value) => print(value)
    case Plus(lhs, rhs) => { print("("); this(lhs); print(" + "); this(rhs); print(")") }
    case Minus(lhs, rhs) => { print("("); this(lhs); print(" - "); this(rhs); print(")") }
    case Times(lhs, rhs) => { print("("); this(lhs); print(" * "); this(rhs); print(")") }
    case Division(lhs, rhs) => { print("("); this(lhs); print(" / "); this(rhs); print(")") }
    case Modulo(lhs, rhs) => { print("("); this(lhs); print(" % "); this(rhs); print(")") }
    case Equals(lhs, rhs) => { print("("); this(lhs); print(" == "); this(rhs); print(")") }
    case GreaterThan(lhs, rhs) => { print("("); this(lhs); print(" > "); this(rhs); print(")") }
    case LessThan(lhs, rhs) => { print("("); this(lhs); print(" < "); this(rhs); print(")") }
    case And(lhs, rhs) => { print("("); this(lhs); print(" && "); this(rhs); print(")") }
    case Or(lhs, rhs) => { print("("); this(lhs); print(" || "); this(rhs); print(")") }
    case Neg(expr) => { print("-"); this(expr) }
    case Not(expr) => { print("!"); this(expr) }
  }
}
