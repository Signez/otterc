package otterc
package analyzer

trait TypeChecker {
  self: Reporter =>

  import Symbols._
  import Types._
  import parser.Trees._

  def createType(typeT: TypeTree): Type = {
    typeT match {
      case IntType() => TInt
      case BoolType() => TBoolean
      case StringType() => TString
      case IntArrayType() => TIntArray
      case id@Identifier(_) => TObject(id.getSymbol.asInstanceOf[ClassSymbol])
      case _ =>
        sys.error("Unexpected Type discovered!")
        null
    }
  }

  /** Typechecking does not produce a value, but has the side effect of
    * attaching types to trees and potentially outputting error messages. */
  def typeCheck(prog: Program, gs: GlobalScope): Unit = {
    var currentMethod: MethodSymbol = null

    def tcOverloadedOperator(expr: ExpressionTree,
                             lhs: ExpressionTree,
                             rhs: ExpressionTree,
                             methodName: String,
                             humanName: String): Type = {
        lhs.getType match {
            case TObject(classSymbol) =>
              classSymbol.lookupMethod(methodName, List(rhs.getType)) match {
                case Some(ms) =>
                  tcExpr(MethodCall(lhs, Identifier(methodName).setPosition(expr), List(rhs)), ms.getType)
                case None =>
                  error("Class '" + classSymbol.name + "' doesn't have any '" + methodName + "' method, " +
                        "but instance is used in " + humanName + " at position " + lhs.posString)
                  TError
              }
            case _ =>
              error("Unexpected types " + lhs.getType + " and " + rhs.getType + " for " + humanName + ", " +
                    "at position " + expr.posString)
              TString
        }
    }

    /** Suggested inner function:
      *
      * Computes the type of an expression. If exp is not empty, checks that
      * the expression is a subtype of one in exp. If it's not, prints an
      * error message and returns the first element of exp. Returning a valid
      * type despite the error is a way to do error recovering: type checking
      * will continue, assuming the correct type was found. */
    def tcExpr(expr: ExpressionTree, exp: Type*): Type = {
      val expList = exp.toList

      val computedType: Type = expr match {
        case Plus(lhs, rhs) =>
          val lt = tcExpr(lhs, TAny)
          val rt = tcExpr(rhs, TAny)
          (lt, rt) match {
            case (TInt, TInt) => TInt
            case (TString, TInt) => TString
            case (TInt, TString) => TString
            case (TString, TString) => TString
            case _ => tcOverloadedOperator(expr, lhs, rhs, "plus", "addition")
          }

        case Minus(lhs, rhs) =>
          val lt = tcExpr(lhs, TAny)
          val rt = tcExpr(rhs, TAny)
          (lt, rt) match {
            case (TInt, TInt) => TInt
            case _ => tcOverloadedOperator(expr, lhs, rhs, "minus", "subtraction")
          }

        case Multiply(lhs, rhs) =>
          val lt = tcExpr(lhs, TAny)
          val rt = tcExpr(rhs, TAny)
          (lt, rt) match {
            case (TInt, TInt) => TInt
            case _ => tcOverloadedOperator(expr, lhs, rhs, "multiply", "multiplication")
          }

        case Divide(lhs, rhs) =>
          val lt = tcExpr(lhs, TAny)
          val rt = tcExpr(rhs, TAny)
          (lt, rt) match {
            case (TInt, TInt) => TInt
            case _ => tcOverloadedOperator(expr, lhs, rhs, "divide", "division")
          }

        case Or(lhs, rhs) =>
          val lt = tcExpr(lhs, TAny)
          val rt = tcExpr(rhs, TAny)
          (lt, rt) match {
            case (TBoolean, TBoolean) => TBoolean
            case _ => tcOverloadedOperator(expr, lhs, rhs, "or", "OR-operation")
          }

        case And(lhs, rhs) =>
          val lt = tcExpr(lhs, TAny)
          val rt = tcExpr(rhs, TAny)
          (lt, rt) match {
            case (TBoolean, TBoolean) => TBoolean
            case _ => tcOverloadedOperator(expr, lhs, rhs, "and", "AND-operation")
          }

        case LesserThan(lhs, rhs) =>
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TBoolean

        case Equals(lhs, rhs) =>
          val lt = tcExpr(lhs, TAny)
          val rt = tcExpr(rhs, TAny)
          if (lt.isSubTypeOf(anyObject) && rt.isSubTypeOf(anyObject)) {
            TBoolean
          } else if (lt == rt) {
            TBoolean
          } else {
            error("Unexpected different types around == operator (" + lt + " and " + rt + ")" +
              " at position " + expr.posString)
            TError
          }

        case Index(lhs, rhs) =>
          tcExpr(lhs, TIntArray)
          tcExpr(rhs, TInt)

        case Length(expr) =>
          tcExpr(expr, TIntArray)
          TInt

        case Not(expr) =>
          tcExpr(expr, TBoolean)

        case MethodCall(objectExpr, methodId, expressions) =>
          val objType = tcExpr(objectExpr, anyObject)

          objType match {
            case TObject(classSymbol) =>
              for (localExpr <- expressions) {
                tcExpr(localExpr, TAny)
              }

              val typeArgList = expressions.map(expr => expr.getType)

              classSymbol.lookupMethod(methodId.name, typeArgList) match {
                case Some(ms) =>
                  methodId.setSymbol(ms)

                  ms.getType
                case None =>
                  error("Unknown method " + methodId.name + "(" + typeArgList.mkString(", ") + ") in class '" + classSymbol.name + "' " +
                    "called at position " + methodId.posString)

                  TError
              }
            case _ =>
              error("Unexpected method call on a non-object of type " + objType +
                " at position " + objectExpr.posString)
              TError
          }


        case IntegerLiteral(value) => TInt
        case StringLiteral(value) => TString
        case BooleanLiteral(value) => TBoolean

        case NewArray(length) =>
          tcExpr(length, TInt)
          TIntArray

        case NewObject(objectId) =>
          TObject(gs.lookupClass(objectId.name).get)

        case thisO@ThisObject() =>
          if (currentMethod != null) {
            TObject(currentMethod.classSymbol)
          } else {
            error("Using `this` keyword outside a method at position " + expr.posString)
            TError
          }

        case id@Identifier(_) =>
          id.getSymbol.asInstanceOf[VariableSymbol].getType

        case Assignment(id, rhs) =>
          tcExpr(rhs, id.getType)

        case IndexAssignment(id, index, rhs) =>
          tcExpr(id, TIntArray)
          tcExpr(index, TInt)
          tcExpr(rhs, TInt)
      }

      if (expList.exists(expected => computedType.isSubTypeOf(expected))) {
        expr match {
          case exp@Identifier(_) => // Don't do anything, it's already done before
          case _ => expr.setType(computedType)
        }
        computedType
      } else {
        if (computedType != TError) {
          error("Unexpected " + computedType + ", expecting " + expList.mkString(" or ") +
            " at position " + expr.posString)
        }
        expList.head
      }
    }

    /** for statements... */
    def tcStat(stat: StatementTree): Unit = {
      stat match {
        case Block(stats) => stats.foreach(tcStat(_))
        case If(condition, statements, elze) =>
          tcExpr(condition, TBoolean)
          tcStat(statements)
          elze match {
            case Some(e) => tcStat(e)
            case None =>
          }
        case While(condition, loop) =>
          tcExpr(condition, TBoolean)
          tcStat(loop)
        case PrintLn(expr) =>
          tcExpr(expr, TString, TInt, TBoolean)
        case UnitExpression(expr) =>
          tcExpr(expr, TAny)
      }
    }

    for (clazz <- prog.classes) {
      for (method <- clazz.methods) {
        currentMethod = method.getSymbol

        if (currentMethod.classSymbol.parent.isDefined) {
          val parent = currentMethod.classSymbol.parent.get
          val parentMethod = parent.lookupMethod(method.id.name, currentMethod.argList.map(arg => arg.getType))

          if (parentMethod.isDefined) {
            // Tests if overridden method has the same return type
            if (currentMethod.getType != parentMethod.get.getType) {
              error("Unexpected " + currentMethod.getType + " return type for method '" + method.id.name + "', " +
                "expecting " + parentMethod.get.getType + " (overridden method exact type) " +
                "at position " + currentMethod.posString)
            }
          }
        }

        method.statements.foreach(tcStat(_))

        if (method.returnExpr.isDefined)
          tcExpr(method.returnExpr.get, currentMethod.getType)
        else
          TUnit
      }
    }

    currentMethod = new MethodSymbol("main", prog.main.getSymbol)
    tcStat(prog.main.stat)
  }
}
