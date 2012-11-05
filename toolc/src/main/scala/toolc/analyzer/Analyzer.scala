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

  private def setSymbols(prog: Program, gs: GlobalScope): Unit = {
    
  }
}
