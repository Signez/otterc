package toolc
package analyzer

trait Analyzer {
  self: Reporter =>

  import parser.Trees._
  import Symbols._

  def analyzeSymbols(prog: Program): GlobalScope = {
    val gs = collectSymbols(prog)
    terminateIfErrors
//    setSymbols(prog, gs)
    gs
  }

  private def collectSymbols(prog: Program): GlobalScope = { 
    var gs = new GlobalScope;
    
    val symbol = new ClassSymbol(prog.main.id.value);
    gs.mainClass = symbol;
    
    gs.classes ++= prog.classes.map(aClass => {
      val classSymbol = new ClassSymbol(aClass.id.value);
      
      if(aClass.extendz.isDefined) {
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
  

  //go through the parse tree and 
  //- connects classes and variables instances to their declaration
  //- connects method calls to the method's declaration
  //- connects "this" to the current class
  //- connects Class ID's to their class declaration
  private def setSymbols(prog: Program, gs: GlobalScope): Unit = {
    var classSymbol = new ClassSymbol("")
    var methodSymbol = new MethodSymbol("", classSymbol)
    
    //go through a statement
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
          setSymbolToIdentifier(id)
          setSymbolsInExpression(expr)
        case _ => sys.error("Unknown Statement discovered!");
      }
    }
    
    //go through an expression
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
          setSymbolsInExpression(objectId)
          //method defined in class
          classSymbol.lookupMethod(methodId.value) match {
	        case Some(vs) => methodId.setSymbol(vs)
	        case None => sys.error("Unknown Method discovered!");
	      }
          
        case IntegerLiteral(value) =>
        case StringLiteral(value) =>
        case BooleanLiteral(value) =>
          
        case NewArray(length)  =>
        case NewObject(objectId)  => 
          gs.lookupClass(objectId.value) match {
            case Some(cs) => objectId.setSymbol(cs)
            case None => sys.error("Unknown Class discovered!");
          } 
          
        case thisO @ ThisObject() => thisO.setSymbol(classSymbol)
          
        case id @ Identifier(_) => setSymbolToIdentifier(id)

        case _ => sys.error("Unknown Expression discovered!");
      }
      
    }
    
    //search identifier declaration of current identifier
    def setSymbolToIdentifier(id: Identifier) : Unit = {
      //identifier defined in method
	  methodSymbol.lookupVar(id.value) match {
	    case Some(vs) => id.setSymbol(vs)
	    case None =>
	      //identifier defined in class
	      classSymbol.lookupVar(id.value) match {
	        case Some(vs) => id.setSymbol(vs)
	        case None =>
	          sys.error("Unknown Identifier discovered!");
	      }
	  }
    }
    
    //go through all classes
    for (classDecl <- prog.classes) {
      classSymbol = 
        gs.lookupClass(classDecl.id.value) match {
          case Some(cS) => cS;
          case None => sys.error("Class ID hasn't been found!");
        }
      //go through all methods in class
      for (methodDecl <- classDecl.methods) { 
        methodSymbol = 
          classSymbol.lookupMethod(methodDecl.id.value) match {
            case Some(cS) => cS;
            case None => sys.error("Method ID hasn't been found!");
          }
        //go through all statements
        methodDecl.statements.foreach(setSymbolsInStatement(_));
      }
    }
  }
}
