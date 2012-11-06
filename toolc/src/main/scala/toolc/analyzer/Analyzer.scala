package toolc
package analyzer

trait Analyzer {
  self: Reporter =>

  import parser.Trees._
  import Symbols._

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
    
    
    val mainClassSymbol = new ClassSymbol(prog.main.id.value);
    
    prog.main.id.setSymbol(mainClassSymbol);
    
    gs.mainClass = mainClassSymbol;
    
    gs.classes ++= prog.classes.map(aClass => {
      val classSymbol = new ClassSymbol(aClass.id.value);
      
      if(aClass.extendz.isDefined) {
      	classSymbol.parent = Some(gs.classes.getOrElse(aClass.extendz.get.value,
      							  new ClassSymbol(aClass.extendz.get.value)));
      }
      
      classSymbol.methods ++= aClass.methods.map(method => {
        val methodSymbol = new MethodSymbol(method.id.value, classSymbol);
        
        methodSymbol.params ++= method.arguments.map(variable => {
          val variableSymbol = new VariableSymbol(variable.id.value);
          
          variable.id.setSymbol(variableSymbol);
          
          variable.id.value -> variableSymbol;
        })
        
        methodSymbol.members ++= method.variables.map(member => {
          val memberSymbol = new VariableSymbol(member.id.value);
          
          member.id.setSymbol(memberSymbol);
          
          member.id.value -> memberSymbol;
        })
        
        methodSymbol.argList ++= method.variables.map(member => {
          val variableSymbol = new VariableSymbol(member.id.value);
          
          variableSymbol;
        })
        
        method.id.setSymbol(methodSymbol);
        
        method.id.value -> methodSymbol
      })
      
      classSymbol.members ++= aClass.variables.map(variable => {
        val variableSymbol = new VariableSymbol(variable.id.value);
          
        variable.id.setSymbol(variableSymbol);
        
        variable.id.value -> variableSymbol;
      })
      
      aClass.id.setSymbol(classSymbol);
      
      aClass.id.value -> classSymbol;
    })
    
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
    var classSymbol = new ClassSymbol("")
    var methodSymbol = new MethodSymbol("", classSymbol)
    
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
          //method defined in class
          classSymbol.lookupMethod(methodId.value) match {
	        case Some(vs) => methodId.setSymbol(vs)
	        case None => error("Unknown method '" + methodId.value + "' found at position " + methodId.posString);
	      }
          expressions.map(expr => setInExpr(expr))
          
        case IntegerLiteral(value) =>
        case StringLiteral(value) =>
        case BooleanLiteral(value) =>
          
        case NewArray(length)  =>
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
	    case None =>
	      //identifier defined in class
	      classSymbol.lookupVar(id.value) match {
	        case Some(vs) => id.setSymbol(vs)
	        case None =>
	          sys.error("Unknown variable identifier '" + id.value + "' found at position " + id.posString);
	      }
	  }
    }
    
    //go through all classes
    for (classDecl <- prog.classes) {
      classSymbol = 
        gs.lookupClass(classDecl.id.value) match {
          case Some(cS) => cS;
          case None => sys.error("Unknown class '" + classDecl.id.value + "' at position " + classDecl.id.posString);
        }
      //go through all methods in class
      for (methodDecl <- classDecl.methods) { 
        methodSymbol = 
          classSymbol.lookupMethod(methodDecl.id.value) match {
            case Some(cS) => cS;
            case None => sys.error("Unknown method '" + methodDecl.id.value + "' at position " + methodDecl.id.posString);
          }
        
        // Analyzing types in members (variables)
        methodDecl.variables.map(variable => {
          variable.theType match {
	          case id @ Identifier(value) =>
	            gs.lookupClass(value) match {
	              case Some(classSym) => id.setSymbol(classSym);
	              case None => error("Unknown type '" + value + "' found at position " + id.posString);
	            }
	          case _ =>
          }
        })
        
        // Analyzing types in arguments
        methodDecl.arguments.map(param => {
          param.theType match {
	          case id @ Identifier(value) =>
	            gs.lookupClass(value) match {
	              case Some(classSym) => id.setSymbol(classSym);
	              case None => error("Unknown type '" + value + "' found at position " + id.posString);
	            }
	          case _ =>
          }
        })
        
        //go through all statements
        methodDecl.statements.foreach(setInStat(_));
        
        methodDecl.returnType match {
          case id @ Identifier(value) =>
            gs.lookupClass(value) match {
              case Some(classSym) => id.setSymbol(classSym)
              case None => error("Unknown return type '" + value + "' found at position " + id.posString);
            }
          case _ => 
        }
        
        setInExpr(methodDecl.returnExpr);
      }
    }
    
    setInStat(prog.main.stat);
  }
}
