package toolc

object TreePrinter {
  import parser.Trees._

  val NEWLINE: String = "\n"

  /** TreePrinter(tree) will produce the same result as before. */
  def apply: (Tree => String) = apply(false)_

  /** TreePrinter.withSymbolIDs(tree) will print the tree with the IDs. */
  def withSymbolIDs: (Tree => String) = apply(true)_

  def apply(withSymbolIDs: Boolean)(t: Tree): String = {
    def printTree(t: Tree, level: Int): String = {
      val leftWhiteSpace: String = " " * level

      def printIdValue(id: Identifier): String = {
        if (withSymbolIDs) {
          try {
            id.getSymbol.name + "#" + id.getSymbol.id
          } catch {
            case e: java.lang.RuntimeException => id.value + "#??"
          }
        } else id.value
      }

      val code: String =
        t match {
          //MainObject ( ClassDeclaration )* <EOF>
          case Program(main, classes) =>
            val strMain: String = printTree(main, level)
            val strClasses: String =
              (for { cls: ClassDecl <- classes } yield printTree(cls, level)).mkString
            return strMain + NEWLINE + strClasses

          //object Identifier { ...
          case MainObject(id, stat) =>
            return "object " + printIdValue(id) + " { " + NEWLINE +
              "  def main(): Unit = { " + NEWLINE +
              printTree(stat, level + 2) +
              "  }" + NEWLINE +
              "}" + NEWLINE

          //class Identifier ( extends Identifier )? { ...
          case ClassDecl(id, extendz, variables, methods) =>
            val strClass: String = "class " + printIdValue(id)
            val strExtendz: String =
              if (extendz.isDefined) " extends " + extendz.get.value else ""
            val strVariables: String =
              (for { variable: VarDecl <- variables } yield printTree(variable, level + 2)).mkString
            val strMethods: String =
              (for { method: MethodDecl <- methods } yield printTree(method, level + 2)).mkString
            return strClass + strExtendz + " {" +
              NEWLINE + strVariables + strMethods + "}" + NEWLINE

          //var Identifier : Type ;
          case VarDecl(id, theType) =>
            return leftWhiteSpace + "var " + printIdValue(id) + ": " + printTree(theType, 0) + ";" + NEWLINE

          //def Identifier ( ( Identifier : Type ( , Identifier : Type )* )? ) : Type = { ...
          case MethodDecl(id, arguments, returnType, variables, statements, returnExpr) =>
            val strArguments: String =
              (for { argument: VarDecl <- arguments }
                yield argument match { case VarDecl(id, theType) => printIdValue(id) + ": " + printTree(theType, 0) }).mkString(", ")
            val strType: String = printTree(returnType, 0)
            val strVariables: String =
              (for { variable: VarDecl <- variables } yield printTree(variable, level + 2)).mkString
            val strStatements: String =
              (for { stat: StatTree <- statements } yield printTree(stat, level + 2)).mkString
            return leftWhiteSpace + "def " + printIdValue(id) + "(" + strArguments + ") : " +
              strType + " = { " + NEWLINE + strVariables + strStatements +
              leftWhiteSpace + " " * 2 + "return " + printTree(returnExpr, 0) + ";" + NEWLINE +
              leftWhiteSpace + "}" + NEWLINE

          //{ ( Statement )* }
          case Block(stats) =>
            if (stats.length > 1) {
              val strStatements =
                (for { stat: StatTree <- stats } yield printTree(stat, level + 2)).mkString
              return leftWhiteSpace + "{" + NEWLINE + strStatements +
                leftWhiteSpace + "}" + NEWLINE
            } else if (stats.length == 0) {
              return leftWhiteSpace + "{}";
            } else {
              return printTree(stats.head, level);
            }

          //if ( Expression ) Statement ( else Statement )?
          case If(condition, then, elze) =>
            val strIf: String = leftWhiteSpace + "if(" + printTree(condition, 0) + ")" +
              NEWLINE + printTree(then, level + 2)
            val strElze: String =
              if (elze.isInstanceOf[Some[ExprTree]])
                leftWhiteSpace + "else " + NEWLINE + printTree(elze.get, level + 2)
              else
                "";
            return strIf + strElze +
              leftWhiteSpace + "/* end if */" + NEWLINE

          //while ( Expression ) Statement
          case While(condition, loop) =>
            val strCondition = printTree(condition, 0)
            val strStatement = printTree(loop, level + 2)
            return leftWhiteSpace + "while (" + strCondition + ")" + NEWLINE +
              strStatement

          //println ( Expression ) ;
          case PrintLn(expr) =>
            return leftWhiteSpace + "println(" + printTree(expr, 0) + ");" + NEWLINE

          //Identifier = Expression ;
          case Assignment(id, expr) =>
            var strIdent = printIdValue(id) + " = "
            return leftWhiteSpace + strIdent + printTree(expr, level + strIdent.size) + ";" + NEWLINE

          //Identifier [ Expression ] = Expression ;
          case IndexAssignment(id, index, expr) =>
            return leftWhiteSpace + printIdValue(id) + "[" + printTree(index, 0) + "] = " +
              printTree(expr, 0) + ";" + NEWLINE

          //Expression ( arguments.. ) => { varDecl; statemenets; }
          case FuncExpr(args, vars, stats, returnExpr) =>
            val strArguments =
            (for (VarDecl(id, theType) <- args)
              yield printTree(id, 0) + ": " + printTree(theType, 0)).mkString("(", ", ", ")")
            val strVariables: String =
              (for { variable: VarDecl <- vars } yield printTree(variable, level + 2)).mkString
            val strStatements: String =
              (for { stat: StatTree <- stats } yield printTree(stat, level + 2)).mkString
            val strType: String = printTree(returnExpr, 0)
            return strArguments + " => " + "{" + NEWLINE + strVariables + strStatements +
              leftWhiteSpace + " " * 2 + "return " + printTree(returnExpr, 0) + ";" + NEWLINE +
              leftWhiteSpace + "}"

          //Expression func( arguments; )
          case FuncCall(funcId, exprList) =>
            printTree(funcId, 0) + exprList.map(printTree(_, 0)).mkString("(", ", ", ")")

          //Expression ( && | || | == | < | + | - | * | / ) Expression
          case Plus(lhs, rhs) => "(" + printTree(lhs, 0) + " + " + printTree(rhs, 0) + ")"
          case Minus(lhs, rhs) => "(" + printTree(lhs, 0) + " - " + printTree(rhs, 0) + ")"
          case Multiply(lhs, rhs) => "(" + printTree(lhs, 0) + " * " + printTree(rhs, 0) + ")"
          case Divide(lhs, rhs) => "(" + printTree(lhs, 0) + " / " + printTree(rhs, 0) + ")"
          case Or(lhs, rhs) => "(" + printTree(lhs, 0) + " || " + printTree(rhs, 0) + ")"
          case And(lhs, rhs) => "(" + printTree(lhs, 0) + " && " + printTree(rhs, 0) + ")"
          case Equals(lhs, rhs) => "(" + printTree(lhs, 0) + " == " + printTree(rhs, 0) + ")"
          case LesserThan(lhs, rhs) => "(" + printTree(lhs, 0) + " < " + printTree(rhs, 0) + ")"

          //Expression [ Expression ]
          case Index(lhs, rhs) => printTree(lhs, 0) + "[" + printTree(rhs, 0) + "]"

          //Expression . length
          case Length(expr) => printTree(expr, 0) + ".length"

          //Expression . Identifier ( ( Expression ( , Expression )* )? )
          case MethodCall(objectId, methodId, expressions) => {
            val strExpressions =
              (for { expr: ExprTree <- expressions } yield printTree(expr, level + 2)).mkString(", ")
            return printTree(objectId, 0) + "." + printIdValue(methodId) + "(" + strExpressions + ")"
          }

          //! Expression
          case Not(expr) => "!(" + printTree(expr, 0) + ")"

          //<INTEGER_LITERAL>
          case IntegerLiteral(value) => value.toString

          // "<STRING_LITERAL>"
          case StringLiteral(value) => "\"" + value + "\""

          //true or false
          case BooleanLiteral(value) =>
            if (value) "true" else "false"

          //Identifier
          case id @ Identifier(value) => {
            printIdValue(id)
          }

          //this
          case ThisObject() => "this"

          //new Int [ Expression ]
          case NewArray(length) => "new Int[" + printTree(length, 0) + "]"

          //new Identifier ( )
          case NewObject(objectId) => "new " + printIdValue(objectId) + "()"

          case BoolType() => "Bool"
          case IntArrayType() => "Int[]"
          case IntType() => "Int"
          case StringType() => "String"
          case FuncType(args, _, returnType) =>
            args.map(printTree(_, 0)).mkString("(", ", ", ")") + " => " + printTree(returnType, 0)
        }
      return code
    }
    /* construct and return the appropriate string ... */
    printTree(t, 0)
  }
}
