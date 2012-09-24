package whilelang

import scala.collection.mutable.Map

/**
 * Interprets Tree content and run actual code.
 */
class Interpreter {
  var registry = Map[String, Int]();
  
  /**
   * Run the interpreter. 
   * 
   * This method is simple, as its only purpose is to reset its variables and to
   * print start/end messages before and after an actual evaluation of the statement.
   */
  def run(stat: Statement): Unit = {
    println("debug: Interpreter started.")
    
    registry.clear() // Clear all variables (useful when this interpreter is used multiple times)
    eval(stat)
    
    println("debug: Interpreter ended.")
  }
  
  /**
   * Evaluate a statement.
   */
  def eval(stat: Statement): Unit = {
    // Depending on statement type, let's do according actions
    stat match {
      
      // wl> println(message, varID);
      case Print (message, varID) => {
        Console.println(message + eval(Var(varID)));
      }
      
      // wl> varID = value;
      case Assign (varID, value) => {
        registry(varID) = eval(value)
      }
      
      // wl> if(expr) { then } else { elze };
      case IfThenElse (expr, then, elze) => {
        if(eval(expr) != 0) {
          eval(then)
        } else {
          eval(elze)
        }
      }
      
      // wl> while(expr) { body };
      case While(expr, body) => {
        while(eval(expr) != 0) {
        	eval(body)          
        }
      }
      
      // wl> { body };
      case Block(body) => body.foreach(eval)
      
      // wl> for(init; expr; step) { body };
      case For(init, expr, step, body) => {
        eval(init)
        eval(While(expr, Block(List(body, step))))
      } 
      
      // wl> ;
      case Skip => ; // Nothing to do here
    }
  }
  
  /**
   * Evaluate an expression.
   * 
   * An expression in While Language returns an integer when evaluated.
   * Booleans are represented by a zero (false) and ones (true), although
   * any non-zeros values are casted to true values.
   * 
   * Note: lhe and rhe stands for Left Hand and Right Hand Expressions.
   */
  def eval(expr: Expression): Int = {
    expr match {
        // Literals
	    
    	case Var(varID) => registry.getOrElse(varID, 0)
	    case IntLiteral(value) => value
	    
	    // Non-boolean, two operands expressions
	    
	    case Plus(lhe, rhe) => { eval(lhe) + eval(rhe) }
	    case Minus(lhe, rhe) => { eval(lhe) - eval(rhe) }
	    case Times(lhe, rhe) => { eval(lhe) * eval(rhe) }
	    case Division(lhe, rhe) => { eval(lhe) / eval(rhe) }
	    case Modulo(lhe, rhe) => { eval(lhe) % eval(rhe) }
	    
	    // Non-boolean, one operand expression
	    
	    case Neg(expr) => 0 - eval(expr)   
	    
	    // Boolean, two operands expressions returning true
	    
	    case Equals(lhe, rhe) if eval(lhe) == eval(rhe) => 1
	    case GreaterThan(lhe, rhe) if eval(lhe) > eval(rhe) => 1
	    case LessThan(lhe, rhe) if eval(lhe) < eval(rhe) => 1
	    case And(lhe, rhe) if eval(lhe) != 0 && eval(rhe) != 0 => 1
	    case Or(lhe, rhe) if eval(lhe) != 0 || eval(rhe) != 0 => 1
	    case Not(expr) if eval(expr) == 0 => 1
	    
	    // Expressions returning false (= everything else)
	    
	    case _ => 0
	}
  }
}
