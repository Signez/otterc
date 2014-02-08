package otterc
package analyzer

import scala.annotation.tailrec

/**
 * Tree analyzer.
 *
 * Analyzer main job is to collect and set accurately symbols (that is,
 * methods, variables and class identifier labels).
 */
trait Analyzer {
  self: Reporter =>

  import parser.Trees._
  import Symbols._
  import Types._

  def analyzeSymbols(prog: Program): GlobalScope = {
    val gs = collectSymbols(prog)
    terminateIfErrors()
    setSymbols(prog, gs)
    gs
  }

  /**
   * Collect all symbols that are declared (classes, methods, params, members).
   */
  private def collectSymbols(prog: Program): GlobalScope = {
    // Creates the global context that we'll populate in this method
    val gs = new GlobalScope

    var alreadyCollectedClasses = List[ClassDecl]()

    def findClassDecl(needle: String): Option[ClassDecl] = {
      for (clazz <- prog.classes) {
        if (needle == clazz.id.name)
          return Some(clazz)
      }
      None
    }

    @tailrec
    def checkForCycles(current: ClassDecl, encountered: List[String]) {
      current.extendz match {
        case Some(parent) =>
          if (encountered.contains(parent.name))
            error("Inheritance graph has cycling ('" + current.id.name + "' extends '" + parent.name + "' that already extends it).")
          else {
            findClassDecl(parent.name) match {
              case Some(parentClass) => checkForCycles(parentClass, current.id.name :: encountered)
              case None => fatalError("Unknown class '" + parent.name + "' found at position " + parent.posString)
            }
          }

        case None => // Nothing
      }
    }

    def collectClass(clazz: ClassDecl): Unit = {
      if (alreadyCollectedClasses.exists(_ eq clazz)) return

      if (gs.classes.contains(clazz.id.name)) {
        error("Unexpected redeclaration for class '" + clazz.id.name + "' at position " + clazz.posString +
          " (previously declared at " + gs.classes.get(clazz.id.name).get.posString + ")")
      }

      if (gs.mainClass.name == clazz.id.name) {
        error("Unexpected class '" + clazz.id.name + "' with the same name as the Main object, at position " + clazz.posString)
      }

      val classSymbol = new ClassSymbol(clazz.id.name)

      checkForCycles(clazz, List())

      classSymbol.parent = clazz.extendz match {
        case Some(parentId) =>
          val parent = findClassDecl(parentId.name).get
          if (parent != clazz) {
            // Not parent of itself
            collectClass(parent)
            Some(gs.classes(parentId.name))
          } else None
        case None => None
      }

      for (method <- clazz.methods) {
        val methodSymbol = new MethodSymbol(method.id.name, classSymbol)

        method.arguments = method.arguments.map({ param =>
          if (methodSymbol.params.contains(param.id.name)) {
            error("Unexpected redeclaration for parameter '" + param.id.name + "' at position " + param.posString +
              " (previously declared at " + methodSymbol.params.get(param.id.name).get.posString + ")")
          }

          val variableSymbol = new VariableSymbol(param.id.name)
          variableSymbol.parentSymbol = methodSymbol

          variableSymbol.setPosition(param)
          param.setSymbol(variableSymbol)
          param.id.setSymbol(variableSymbol)

          methodSymbol.params += param.id.name -> variableSymbol
          methodSymbol.argList ::= variableSymbol

          param
        })

        methodSymbol.argList = methodSymbol.argList.reverse

        for (member <- method.variables) {
          if (methodSymbol.members.contains(member.id.name)) {
            error("Unexpected redeclaration for local variable '" + member.id.name + "' at position " + member.posString +
              " (previously declared at " + methodSymbol.members.get(member.id.name).get.posString + ")")
          }

          if (methodSymbol.params.contains(member.id.name)) {
            error("Unexpected shadowing local variable declaration '" + member.id.name + "' at position " + member.posString +
              " (shadows parameter declared at " + methodSymbol.params.get(member.id.name).get.posString + ")")
          }

          val memberSymbol = new VariableSymbol(member.id.name)
          memberSymbol.parentSymbol = methodSymbol

          memberSymbol.setPosition(member)
          member.setSymbol(memberSymbol)
          member.id.setSymbol(memberSymbol)

          methodSymbol.members += member.id.name -> memberSymbol
        }

        methodSymbol.setPosition(method)
        method.setSymbol(methodSymbol)
        method.id.setSymbol(methodSymbol)

        classSymbol.methodsSet += methodSymbol
      }

      for (variable <- clazz.variables) {
        if (classSymbol.members.contains(variable.id.name)) {
          error("Unexpected member variable redeclaration '" + variable.id.name + "' found at " + variable.posString +
            " (previously declared at '" + classSymbol.members.get(variable.id.name).get.posString + "')")
        }

        val variableSymbol = new VariableSymbol(variable.id.name)
        variableSymbol.parentSymbol = classSymbol

        if (classSymbol.parent.isDefined) {
          val pMember = classSymbol.parent.get.lookupVar(variable.id.name)

          if (pMember.isDefined) {
            error("Unexpected overriding member '" + variableSymbol.name + "' found at " + variable.posString +
              " (overrides parent member declared at '" + pMember.get.posString + "')")
          }
        }

        variableSymbol.setPosition(variable)
        variable.setSymbol(variableSymbol)
        variable.id.setSymbol(variableSymbol)

        classSymbol.members += variable.id.name -> variableSymbol
      }


      classSymbol.setPosition(clazz)
      clazz.setSymbol(classSymbol)
      clazz.id.setSymbol(classSymbol)

      alreadyCollectedClasses = clazz :: alreadyCollectedClasses
      gs.classes += clazz.id.name -> classSymbol
    }

    val mainClassSymbol = new ClassSymbol(prog.main.id.name)

    mainClassSymbol.setPosition(prog.main)
    prog.main.setSymbol(mainClassSymbol)
    prog.main.id.setSymbol(mainClassSymbol)

    gs.mainClass = mainClassSymbol

    for (clazz <- prog.classes) {
      collectClass(clazz)
    }

    gs
  }

  private def createType(typeT: TypeTree): Type = {
    typeT match {
      case IntType() => TInt
      case BoolType() => TBoolean
      case StringType() => TString
      case IntArrayType() => TIntArray
      case id@Identifier(_) => TObject(id.getSymbol.asInstanceOf[ClassSymbol])
      case _ =>
        sys.error("Unexpected Type discovered! (" + TreePrinter(withSymbolIDs = true)(typeT) + ")")
        null
    }
  }

  /**
   * Go through the parse tree and connect leafs to the right symbols.
   *
   * - connects classes and variables instances to their declaration
   * - connects method calls to the method's declaration
   * - connects "this" to the current class
   * - connects Class ID's to their class declaration
   */
  private def setSymbols(prog: Program, gs: GlobalScope): Unit = {
    // Initializing context for closures methods "setInStat", "setInExpr", etc.
    var classSymbol: ClassSymbol = null
    var methodSymbol: MethodSymbol = null

    /**
     * Analyze a statement, finding elements that need symbol assignment. 
     */
    def setInStat(statDecl: StatementTree): Unit = {
      statDecl match {
        case Block(stats) => stats.foreach(setInStat)
        case If(condition, statements, elze) =>
          setInExpr(condition)
          setInStat(statements)
          elze match {
            case Some(e) => setInStat(e)
            case None =>
          }
        case While(condition, loop) =>
          setInExpr(condition)
          setInStat(loop)
        case PrintLn(expr) =>
          setInExpr(expr)
        case UnitExpression(expr) =>
          setInExpr(expr)
      }
    }

    //go through an expression
    def setInExpr(exprDecl: ExpressionTree): Unit = {
      exprDecl match {
        case Plus(lhs, rhs) => setInExpr(lhs); setInExpr(rhs)
        case Minus(lhs, rhs) => setInExpr(lhs); setInExpr(rhs)
        case Multiply(lhs, rhs) => setInExpr(lhs); setInExpr(rhs)
        case Divide(lhs, rhs) => setInExpr(lhs); setInExpr(rhs)
        case Or(lhs, rhs) => setInExpr(lhs); setInExpr(rhs)
        case And(lhs, rhs) => setInExpr(lhs); setInExpr(rhs)
        case Equals(lhs, rhs) => setInExpr(lhs); setInExpr(rhs)
        case LesserThan(lhs, rhs) => setInExpr(lhs); setInExpr(rhs)
        case Index(lhs, rhs) => setInExpr(lhs); setInExpr(rhs)
        case Length(expr) => setInExpr(expr)
        case Not(expr) => setInExpr(expr)
        case MethodCall(objectId, methodId, expressions) =>
          setInExpr(objectId)

          // Beware : these lines are commented out, and it's expected
          //          We can't associate yet method calls with right methods 

          //classSymbol.lookupMethod(methodId.value) match {
          //  case Some(vs) => methodId.setSymbol(vs)
          //  case None => error("Unknown method '" + methodId.value + "' found at position " + methodId.posString);
          //}
          expressions.map(expr => setInExpr(expr))

        case IntegerLiteral(value) =>
        case StringLiteral(value) =>
        case BooleanLiteral(value) =>

        case NewArray(length) => setInExpr(length)
        case NewObject(objectId) =>
          gs.lookupClass(objectId.name) match {
            case Some(cs) => objectId.setSymbol(cs)
            case None => error("Unknown class '" + objectId.name + "' found at position " + objectId.posString)
          }

        case thisO@ThisObject() => thisO.setSymbol(classSymbol)

        case id@Identifier(_) => setToVariable(id)

        case Assignment(id, expr) =>
          setToVariable(id)
          setInExpr(expr)
        case IndexAssignment(id, index, expr) =>
          setToVariable(id)
          setInExpr(index)
          setInExpr(expr)
      }

    }

    /**
     * Assign the correct symbol to an identifier that represents a variable.
     */
    def setToVariable(id: Identifier): Unit = {
      //identifier defined in method
      methodSymbol.lookupVar(id.name) match {
        case Some(vs) => id.setSymbol(vs)
        case None => error("Unknown variable identifier '" + id.name + "' found at position " + id.posString)
      }
    }

    //go through all classes
    for (classDecl <- prog.classes) {
      classSymbol =
        gs.lookupClass(classDecl.id.name) match {
          case Some(cS) => cS
          case None => sys.error("Unknown class '" + classDecl.id.name + "' at position " + classDecl.id.posString)
        }

      // Analyzing types in members of class (variables)
      classDecl.variables.foreach(variable => {
        variable.theType match {
          case id@Identifier(value) =>
            gs.lookupClass(value) match {
              case Some(classSym) => id.setSymbol(classSym)
              case None => sys.error("Unknown type '" + value + "' found at position " + id.posString)
            }
          case _ =>
        }
        variable.getSymbol.setType(createType(variable.theType))
      })

      //go through all methods in class
      for (methodDecl <- classDecl.methods) {
        methodSymbol = methodDecl.getSymbol

        // Analyzing types in members (variables)
        methodDecl.variables.foreach(variable => {
          variable.theType match {
            case id@Identifier(value) =>
              gs.lookupClass(value) match {
                case Some(classSym) => id.setSymbol(classSym)
                case None => sys.error("Unknown type '" + value + "' found at position " + id.posString)
              }
            case _ =>
          }
          variable.getSymbol.setType(createType(variable.theType))
        })


        // Analyzing types in arguments
        methodDecl.arguments.foreach(param => {
          param.theType match {
            case id@Identifier(value) =>
              gs.lookupClass(value) match {
                case Some(classSym) => id.setSymbol(classSym)
                case None => sys.error("Unknown type '" + value + "' found at position " + id.posString)
              }
            case _ =>
          }
          param.getSymbol.setType(createType(param.theType))
        })

        val methodTuple = (methodDecl.id.name, methodDecl.arguments.map(param => createType(param.theType)))

        if (classSymbol.lookupMethod(methodTuple._1, methodTuple._2, localOnly = true).isDefined) {
          error("Unexpected redeclaration for method '" + methodSymbol.name + "' at position " + methodDecl.posString +
            " (previously declared at " + classSymbol.methods.get(methodTuple).get.posString + ")")
        }

        if (classSymbol.parent.isDefined) {
          val pMethod = classSymbol.parent.get.lookupMethod(methodTuple._1, methodTuple._2)

          if (pMethod.isDefined && pMethod.get.params.size != methodSymbol.params.size) {
            error("Unexpected overriding method '" + methodSymbol.name + "' found at " + methodDecl.posString +
              " (overrides parent method declared at '" + pMethod.get.posString + "' with wrong number of params)")
          }
        }

        // Analyzing method statements
        methodDecl.statements.foreach(setInStat)

        // Analyzing return type
        methodDecl.returnType match {
          case id@Identifier(value) =>
            gs.lookupClass(value) match {
              case Some(classSym) => id.setSymbol(classSym)
              case None => sys.error("Unknown return type '" + value + "' found at position " + id.posString)
            }
          case _ =>
        }
        methodDecl.getSymbol.setType(createType(methodDecl.returnType))

        if (methodDecl.returnExpr.isDefined) setInExpr(methodDecl.returnExpr.get)

        classSymbol.methods += methodTuple -> methodSymbol
      }
    }

    classSymbol = gs.mainClass
    methodSymbol = new MethodSymbol("main", classSymbol); // Fake and empty method for main
    setInStat(prog.main.stat)
  }
}
