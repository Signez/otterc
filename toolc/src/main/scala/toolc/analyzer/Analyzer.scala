package toolc
package analyzer

trait Analyzer {
  self: Reporter =>

  import parser.Trees._
  import Symbols._
  import Types._

  def analyzeSymbols(prog: Program): GlobalScope = {
    val gs = collectSymbols(prog)
    terminateIfErrors
    setSymbols(prog, gs)
    gs
  }

  /**
   * Collect all symbols that are declared (classes, methods, params, members).
   */
  private def collectSymbols(prog: Program): GlobalScope = {
    // Creates the global context that we'll populate in this method
    var gs = new GlobalScope;

    var alreadyCollectedClasses = List[ClassDecl]();

    def findClassDecl(needle: String): Option[ClassDecl] = {
      for (clazz <- prog.classes) {
        if (needle == clazz.id.value)
          return Some(clazz);
      }
      return None;
    }
    
    def checkForCycles(current: ClassDecl, encountred: List[String]) {
        current.extendz match {
          case Some(parent) =>
            if (encountred.contains(parent.value))
              error("Inheritance graph has cycling ('" + current.id.value + "' extends '" + parent.value + "' that already extends it).");
            else {
              findClassDecl(parent.value) match {
                case Some(parentClass) => checkForCycles(parentClass, current.id.value :: encountred);
                case None => fatalError("Unknown class '" + parent.value + "' found at position " + parent.posString);
              }
            }

          case None => // Nothing
        }
      }

    def collectClass(clazz: ClassDecl): Unit = {
      if (alreadyCollectedClasses.exists(_ eq clazz)) return;

      if (gs.classes.contains(clazz.id.value)) {
        error("Unexpected redeclaration for class '" + clazz.id.value + "' at position " + clazz.posString +
          " (previously declared at " + gs.classes.get(clazz.id.value).get.posString + ")");
      }
      
      if (gs.mainClass.name == clazz.id.value) {
        error("Unexpected class '" + clazz.id.value + "' with the same name as the Main object, at position " + clazz.posString);
      }

      val classSymbol = new ClassSymbol(clazz.id.value);

      checkForCycles(clazz, List());

      classSymbol.parent = clazz.extendz match {
        case Some(parentId) => 
          // Presence of parent class already checked in cycle inheritance avoiding loop (checkForCycles) 
          collectClass(findClassDecl(parentId.value).get)
          Some(gs.classes(parentId.value))
        case None => None
      }

      for (method <- clazz.methods) {
        if (classSymbol.methods.contains(method.id.value)) {
          error("Unexpected redeclaration for method '" + method.id.value + "' at position " + method.posString +
            " (previously declared at " + classSymbol.methods.get(method.id.value).get.posString + ")");
        }

        val methodSymbol = new MethodSymbol(method.id.value, classSymbol);

        for (param <- method.arguments) {
          if (methodSymbol.params.contains(param.id.value)) {
            error("Unexpected redeclaration for parameter '" + param.id.value + "' at position " + param.posString +
              " (previously declared at " + methodSymbol.params.get(param.id.value).get.posString + ")");
          }

          val variableSymbol = new VariableSymbol(param.id.value);
          
          variableSymbol.setPos(param);
          param.setSymbol(variableSymbol);
          param.id.setSymbol(variableSymbol);

          methodSymbol.params += param.id.value -> variableSymbol;
          methodSymbol.argList ::= variableSymbol;
        }
        
        methodSymbol.argList = methodSymbol.argList.reverse
        
        if(classSymbol.parent.isDefined) {
          val pMethod = classSymbol.parent.get.lookupMethod(method.id.value);
          
          if(pMethod.isDefined && pMethod.get.params.size != methodSymbol.params.size) {
            error("Unexpected overriding method '" + methodSymbol.name + "' found at " + method.posString + 
                  " (overrides parent method declared at '" + pMethod.get.posString + "' with wrong number of params)");
          }
        }

        for (member <- method.variables) {
          if (methodSymbol.members.contains(member.id.value)) {
            error("Unexpected redeclaration for local variable '" + member.id.value + "' at position " + member.posString +
              " (previously declared at " + methodSymbol.members.get(member.id.value).get.posString + ")");
          }

          if (methodSymbol.params.contains(member.id.value)) {
            error("Unexpected shadowing local variable declaration '" + member.id.value + "' at position " + member.posString +
              " (shadows parameter declared at " + methodSymbol.params.get(member.id.value).get.posString + ")");
          }

          val memberSymbol = new VariableSymbol(member.id.value);

          memberSymbol.setPos(member);
          member.setSymbol(memberSymbol);
          member.id.setSymbol(memberSymbol);

          methodSymbol.members += member.id.value -> memberSymbol;
        }

        methodSymbol.setPos(method);
        method.setSymbol(methodSymbol);
        method.id.setSymbol(methodSymbol);

        classSymbol.methods += method.id.value -> methodSymbol
      }

      for(variable <- clazz.variables) {
        if(classSymbol.members.contains(variable.id.value)) {
            error("Unexpected member variable redeclaration '" + variable.id.value + "' found at " + variable.posString + 
                  " (previously declared at '" + classSymbol.members.get(variable.id.value).get.posString + "')");
        }
        
        val variableSymbol = new VariableSymbol(variable.id.value);
        
        if(classSymbol.parent.isDefined) {
          val pMember = classSymbol.parent.get.lookupVar(variable.id.value);
          
          if(pMember.isDefined) {
            error("Unexpected overriding member '" + variableSymbol.name + "' found at " + variable.posString + 
                  " (overrides parent member declared at '" + pMember.get.posString + "')");
          }
        }

        variableSymbol.setPos(variable);
        variable.setSymbol(variableSymbol);
        variable.id.setSymbol(variableSymbol);

        classSymbol.members += variable.id.value -> variableSymbol;
      }

      classSymbol.setPos(clazz);
      clazz.setSymbol(classSymbol);
      clazz.id.setSymbol(classSymbol);

      alreadyCollectedClasses = clazz :: alreadyCollectedClasses;
      gs.classes += clazz.id.value -> classSymbol;
    }

    val mainClassSymbol = new ClassSymbol(prog.main.id.value);

    mainClassSymbol.setPos(prog.main);
    prog.main.setSymbol(mainClassSymbol);
    prog.main.id.setSymbol(mainClassSymbol);

    gs.mainClass = mainClassSymbol;

    for (clazz <- prog.classes) {
      collectClass(clazz);
    }

    gs
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
    var classSymbol : ClassSymbol = null;
    var methodSymbol : MethodSymbol = null;
    
    
    def createType(typeT: TypeTree): Type = {
      typeT match {
        case IntType() => TInt
        case BoolType() => TBoolean
        case StringType() => TString
        case IntArrayType() => TIntArray
        case id @ Identifier(_) => TObject(id.getSymbol.asInstanceOf[ClassSymbol])
        case _ =>
          sys.error("Unexpected Type discovered!")
          null
      }
    }
    
    
    /**
     * Analyze a statement, finding elements that need symbol assignment. 
     */
    def setInStat(statDecl: StatTree): Unit = {
      statDecl match {
        case Block(stats) => stats.foreach(setInStat(_)) 
        case If(condition, then, elze) =>
          setInExpr(condition)
          setInStat(then)
          elze match {
            case Some(e) => setInStat(e)
            case None => 
          }
        case While(condition, loop) =>
          setInExpr(condition)
          setInStat(loop)
        case PrintLn(expr) =>
          setInExpr(expr)
        case Assignment(id, expr) =>
          setToVariable(id)
          setInExpr(expr)
        case IndexAssignment(id, index, expr) =>
          setToVariable(id)
          setInExpr(index)
          setInExpr(expr)
      }
    }
    
    //go through an expression
    def setInExpr(exprDecl: ExprTree): Unit = {
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
          
        case NewArray(length)  => setInExpr(length)
        case NewObject(objectId)  => 
          gs.lookupClass(objectId.value) match {
            case Some(cs) => objectId.setSymbol(cs)
            case None => error("Unknown class '" + objectId.value + "' found at position " + objectId.posString);
          } 
          
        case thisO @ ThisObject() => thisO.setSymbol(classSymbol)
          
        case id @ Identifier(_) => setToVariable(id)
      }
      
    }
    
	/**
	 * Assign the correct symbol to an identifier that represents a variable.
	 */
    def setToVariable(id: Identifier) : Unit = {
      //identifier defined in method
	  methodSymbol.lookupVar(id.value) match {
	    case Some(vs) => id.setSymbol(vs)
	    case None => error("Unknown variable identifier '" + id.value + "' found at position " + id.posString);
	  }
    }
    
    //go through all classes
    for (classDecl <- prog.classes) {
      classSymbol = 
        gs.lookupClass(classDecl.id.value) match {
          case Some(cS) => cS;
          case None => sys.error("Unknown class '" + classDecl.id.value + "' at position " + classDecl.id.posString);
        }
      
      // Analyzing types in members of class (variables)
      classDecl.variables.foreach(variable => {
        variable.theType match {
            case id @ Identifier(value) =>
              gs.lookupClass(value) match {
                case Some(classSym) => id.setSymbol(classSym);
                case None => error("Unknown type '" + value + "' found at position " + id.posString);
              }
            case _ =>
        }
        variable.getSymbol.setType(createType(variable.theType));
      })
      
      //go through all methods in class
      for (methodDecl <- classDecl.methods) { 
        methodSymbol = 
          classSymbol.lookupMethod(methodDecl.id.value) match {
            case Some(cS) => cS;
            case None => sys.error("Unknown method '" + methodDecl.id.value + "' at position " + methodDecl.id.posString);
          }
        
        // Analyzing types in members (variables)
        methodDecl.variables.foreach(variable => {
          variable.theType match {
	          case id @ Identifier(value) =>
	            gs.lookupClass(value) match {
	              case Some(classSym) => id.setSymbol(classSym);
	              case None => error("Unknown type '" + value + "' found at position " + id.posString);
	            }
	          case _ =>
          }
          variable.getSymbol.setType(createType(variable.theType));
        })
       
        
        // Analyzing types in arguments
        methodDecl.arguments.foreach(param => {
          param.theType match {
	          case id @ Identifier(value) =>
	            gs.lookupClass(value) match {
	              case Some(classSym) => id.setSymbol(classSym);
	              case None => error("Unknown type '" + value + "' found at position " + id.posString);
	            }
	          case _ =>
          }
          param.getSymbol.setType(createType(param.theType));
        })
        
        // Analyzing method statements
        methodDecl.statements.foreach(setInStat(_));
        
        // Analyzing return type
        methodDecl.returnType match {
          case id @ Identifier(value) =>
            gs.lookupClass(value) match {
              case Some(classSym) => id.setSymbol(classSym)
              case None => error("Unknown return type '" + value + "' found at position " + id.posString);
            }
          case _ => 
        }
        methodDecl.getSymbol.setType(createType(methodDecl.returnType));
        
        setInExpr(methodDecl.returnExpr);
      }
    }
    
    classSymbol = gs.mainClass;
    methodSymbol = new MethodSymbol("main", classSymbol); // Fake and empty method for main
    setInStat(prog.main.stat);
  }
}
