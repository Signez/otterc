package toolc
package analyzer

trait TypeChecker {
  self: Reporter =>

  import Symbols._
  import Types._
  import parser.Trees._

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def typeCheck(prog: Program, gs: GlobalScope): Unit = {
    var currentMethod : MethodSymbol = null;
      
    /** Suggested inner function:
     *
     * Computes the type of an expression. If exp is not empty, checks that
     * the expression is a subtype of one in exp. If it's not, prints an
     * error message and returns the first element of exp. Returning a valid
     * type despite the error is a way to do error recovering: type checking
     * will continue, assuming the correct type was found. */
    def tcExpr(expr: ExprTree, exp: Type*): Type = {
      val expList = exp.toList;
      
      val computedType : Type = expr match {
		 case Plus(lhs, rhs) =>
		     val lt = tcExpr(lhs, TAny)
		     val rt = tcExpr(rhs, TAny)
			 (lt, rt) match {
			   case (TInt, TInt) => TInt
			   case (TString, TInt) => TString
			   case (TInt, TString) => TString
			   case (TString, TString) => TString
			   case _ => 
			     error("Unexpected types " + lhs.getType + " and " + rhs.getType + " for Addition, " +
			   				   "expecting int and string, at position " + expr.posString)
			   	 TString
			 }
		 
		 case Minus(lhs, rhs) =>
			 tcExpr(lhs, TInt) 
			 tcExpr(rhs, TInt)
		 
        case Multiply(lhs, rhs) =>
			 tcExpr(lhs, TInt) 
			 tcExpr(rhs, TInt)
		 
        case Divide(lhs, rhs) =>
			 tcExpr(lhs, TInt) 
			 tcExpr(rhs, TInt)
		 
        case Or(lhs, rhs) => 
			 tcExpr(lhs, TBoolean) 
			 tcExpr(rhs, TBoolean)
		
        case And(lhs, rhs) =>
			 tcExpr(lhs, TBoolean) 
			 tcExpr(rhs, TBoolean)
		
        case LesserThan(lhs, rhs) =>
			 tcExpr(lhs, TInt) 
			 tcExpr(rhs, TInt)
			 TBoolean
		
        case Equals(lhs, rhs) =>
			 val lt = tcExpr(lhs, TAny);
			 val rt = tcExpr(rhs, TAny);
             if(lt.isSubTypeOf(anyObject) && rt.isSubTypeOf(anyObject)) {
               TBoolean
             }else if(lt == rt) {
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
		      classSymbol.lookupMethod(methodId.value) match {
		    	  case Some(ms) => 
		    	    methodId.setSymbol(ms)
		    	    
		    	    val args = ms.argList;
		    	    for((vs, localExpr) <- args.zip(expressions)) {
		    	      tcExpr(localExpr, vs.getType);
		    	    }
		    	    
		    	    ms.getType
		    	  case None => 
		    	    error("Unknown method '" + methodId.value + "' (in class '" + classSymbol.name + "') " +
		    	    	  "called at position " + methodId.posString);
		    	    TError
		      }
		    case _ =>
		      error("Unexpected method call on a non-object of type " + objType +
		      		" at position " + objectExpr.posString);
		      TError
		  }
          
        case IntegerLiteral(value) => TInt
        case StringLiteral(value) => TString
        case BooleanLiteral(value) => TBoolean
          
        case NewArray(length) =>
          tcExpr(length, TInt)
          TIntArray
        
        case NewObject(objectId)  => 
          TObject(gs.lookupClass(objectId.value).get)
          
        case thisO @ ThisObject() => 
          if(currentMethod != null) {
        	 TObject(currentMethod.classSymbol)
          } else {
            error("Using `this` keyword outside a method at position " + expr.posString);
            TError
          }
          
        case id @ Identifier(_) => 
      	  id.getSymbol.asInstanceOf[VariableSymbol].getType
	  }
      
      if(expList.exists(expected => computedType.isSubTypeOf(expected))) {
        expr.setType(computedType);
    	computedType;
      } else {
        if(computedType != TError) {
        	error("Unexpected " + computedType + ", expecting " + expList.mkString(" or ") +
        	      " at position " + expr.posString)
        }
        expList.head
      }
    }

    /** for statements... */
    def tcStat(stat: StatTree): Unit = {
      stat match {
        case Block(stats) => stats.foreach(tcStat(_)) 
        case If(condition, then, elze) =>
          tcExpr(condition, TBoolean)
          tcStat(then)
          elze match {
            case Some(e) => tcStat(e)
            case None => 
          }
        case While(condition, loop) =>
          tcExpr(condition, TBoolean)
          tcStat(loop)
        case PrintLn(expr) =>
          tcExpr(expr, TString, TInt, TBoolean)
        case Assignment(id, expr) =>
          tcExpr(expr, id.getType)
        case IndexAssignment(id, index, expr) =>
          tcExpr(id, TIntArray)
          tcExpr(index, TInt)
          tcExpr(expr, TInt)
      }
    }
    
    for(clazz <- prog.classes) {
      for(method <- clazz.methods) {
        currentMethod = method.getSymbol;
        
        if(currentMethod.classSymbol.parent.isDefined) {
          val parent = currentMethod.classSymbol.parent.get;
          val parentMethod = parent.lookupMethod(method.id.value)
          
          if(parentMethod.isDefined) {
            
            // Tests if overridden method has the same return type
            if(currentMethod.getType != parentMethod.get.getType) {
              error("Unexpected " + currentMethod.getType + " return type for method '" + method.id.value + "', " +
            	    "expecting " + parentMethod.get.getType + " (overridden method exact type) " +
            	    "at position " + currentMethod.posString);
            }
            
            // Tests if overridden method has the same parameters type
            val currentArgs = currentMethod.argList;
            val parentArgs = parentMethod.get.argList;
    	    for((carg, parg) <- currentArgs.zip(parentArgs)) {
    	      if(carg.getType != parg.getType) {
    	        error("Unexpected " + carg.getType + " type for parameter '" + carg.name + "', " +
            	      "expecting " + parg.getType + " (like the '" + parg.name + "' parameter in overridden method) " +
            	      "at position " + carg.posString);
    	      }
    	    }
          }
        }
        
        method.statements.foreach(tcStat(_))
        
        tcExpr(method.returnExpr, currentMethod.getType)
      }
    }
    
    currentMethod = new MethodSymbol("main", prog.main.getSymbol);
    tcStat(prog.main.stat)
  }
}
