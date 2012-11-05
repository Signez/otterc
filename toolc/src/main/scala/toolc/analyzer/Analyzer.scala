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

  private def collectSymbols(prog: Program): GlobalScope = { 
    var gs = new GlobalScope;
    
    val symbol = new ClassSymbol(prog.main.id.value);
    gs.mainClass = symbol;
    
    gs.classes ++= prog.classes.map(aClass => {
      val classSymbol = new ClassSymbol(aClass.id.value);
      
      if(!aClass.extendz.isDefined) {
      	classSymbol.parent = Some(new ClassSymbol(aClass.extendz.get.value));
      }
      
      classSymbol.methods ++= aClass.methods.map(method => {
        val methodSymbol = new MethodSymbol(method.id.value, classSymbol);
        
        methodSymbol.params ++= method.arguments.map(variable => {
          val variableSymbol = new VariableSymbol(variable.id.value);
          
          variable.id.value -> variableSymbol;
        })
        
        methodSymbol.members ++= method.variables.map(member => {
          val variableSymbol = new VariableSymbol(member.id.value);
          
          member.id.value -> variableSymbol;
        })
        
        methodSymbol.argList ++= method.variables.map(member => {
          val variableSymbol = new VariableSymbol(member.id.value);
          
          variableSymbol;
        })
        
        method.id.value -> methodSymbol
      })
      
      classSymbol.members ++= aClass.variables.map(variable => {
        val variableSymbol = new VariableSymbol(variable.id.value);
          
        variable.id.value -> variableSymbol;
      })
      
      aClass.id.value -> classSymbol;
    })
    
    gs
  }
  

  //go through the parse tree and connects classes, methods and variables instances to their symbols
  private def setSymbols(prog: Program, gs: GlobalScope): Unit = {
    var classSymbol = new ClassSymbol("")
    var methodSymbol = new MethodSymbol("", classSymbol)
    
    //go through class
    def setSymbolsInStatement(statDecl: StatTree): Unit = {
      statDecl match {
        case Block(stats) => stats.foreach(setSymbolsInStatement(_)) 
        case If(condition, then, elze) =>
          setSymbolsInExpression(condition)
          setSymbolsInStatement(then)
          elze match {
            case Some(e) => setSymbolsInStatement(e)
            case None => 
          }
        case While(condition, loop) =>
          setSymbolsInExpression(condition)
          setSymbolsInStatement(loop)
        case PrintLn(expr) =>
          setSymbolsInExpression(expr)
        case assign @ Assignment(id, expr) =>
          setSymbolsToIdentifier(id)
          setSymbolsInExpression(expr)
        case _ => sys.error("Unknown Statement discovered!");
      }
    }
    
    def setSymbolsInExpression(exprDecl: ExprTree): Unit = {
      exprDecl match {
        case Plus(lhs, rhs) =>
          setSymbolsInExpression(lhs)
          setSymbolsInExpression(rhs)
        case Minus(lhs, rhs) =>
          setSymbolsInExpression(lhs)
          setSymbolsInExpression(rhs)
        case Multiply(lhs, rhs) =>
          setSymbolsInExpression(lhs)
          setSymbolsInExpression(rhs)
        case Divide(lhs, rhs) =>
          setSymbolsInExpression(lhs)
          setSymbolsInExpression(rhs)
        case Or(lhs, rhs) =>
          setSymbolsInExpression(lhs)
          setSymbolsInExpression(rhs)
        case And(lhs, rhs) =>
          setSymbolsInExpression(lhs)
          setSymbolsInExpression(rhs)
        case Equals(lhs, rhs) =>
          setSymbolsInExpression(lhs)
          setSymbolsInExpression(rhs)
        case LesserThan(lhs, rhs) =>
          setSymbolsInExpression(lhs)
          setSymbolsInExpression(rhs)
        case Index(lhs, rhs) =>
          setSymbolsInExpression(lhs)
          setSymbolsInExpression(rhs)
        case Length(expr) =>
          setSymbolsInExpression(expr)
        case Not(expr) =>
          setSymbolsInExpression(expr)
        case MethodCall(objectId, methodId, expressions) =>
          
        case IntegerLiteral(value) =>
        case StringLiteral(value) =>
        case BooleanLiteral(value) =>
          
        case NewArray(length)  =>
        case NewObject(objectId)  =>
          
        case ThisObject() =>
          
        case id @ Identifier(value) => setSymbolsToIdentifier(id)

        case _ => sys.error("Unknown Expression discovered!");
      }
      
    }
    
    def setSymbolsToIdentifier(id: Identifier): Unit = {
      var varSym =
	    methodSymbol.lookupVar(id.value) match {
	      case vs @ Some(_) => vs
	      case None =>
	        classSymbol.lookupVar(id.value) match {
	          case ms @ Some(_) => ms
	          case None =>
	            
	        }
	    }
      classSymbol.lookupVar(id.value)
    }
    
    setSymbolsInStatement(prog.main.stat);
    for (classDecl <- prog.classes) {
      classSymbol = 
        gs.lookupClass(classDecl.id.value) match {
          case Some(cS) => cS;
          case None => sys.error("Class ID hasn't been found!");
        }
      for (methodDecl <- classDecl.methods) { 
        methodSymbol = 
          classSymbol.lookupMethod(methodDecl.id.value) match {
            case Some(cS) => cS;
            case None => sys.error("Method ID hasn't been found!");
          }
        methodDecl.statements.foreach(setSymbolsInStatement(_));
      }
    }
  }
}
