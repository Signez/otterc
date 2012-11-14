package toolc
package analyzer

object Types {
  import Symbols._

  sealed abstract class Type {
    // we suggest you implement this to make type checking easier
    def isSubTypeOf(tpe: Type): Boolean
  }

  // having this "bottom" class (which extends every other one) can be convenient for error recovery...
  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }

  // the default type for all Typed objects
  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[untyped]"
  }
  
  // Is subtype of everything
  case object TAny extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TAny => true
      case _ => false
    }
    override def toString = "[any]"
  }

  case object TIntArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean =
      tpe match {
        case TAny => true
        case TIntArray => true
        case _ => false
      }
    override def toString = "int[]"
  }
  
  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean =
      tpe match {
        case TAny => true
        case TInt => true
        case _ => false
      }
    override def toString = "int"
  }

  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean =
      tpe match {
        case TAny => true
        case TString => true
        case _ => false
      }
    override def toString = "string"
  }
  
  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean =
      tpe match {
        case TAny => true
        case TBoolean => true
        case _ => false
      }
    override def toString = "boolean"
  }

  // It's convenient to reference classes by their symbol, rather than by their name, to avoid doing the lookup every time.
  case class TObject(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean =
    	tpe match {
        	case TAny => true
        	case `anyObject` => true
    		case TObject(targetSymbol) => (targetSymbol == classSymbol 
    		                              || (classSymbol.parent.isDefined && 
    		      	 					      new TObject(classSymbol.parent.get).isSubTypeOf(tpe)))
    		  	
    		case _ => false
    	}
    override def toString = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses.
  val anyObject = TObject(new ClassSymbol("Object"))

  /** Add this trait to all tree nodes which represent typed expressions.
    * This will be needed for code generation. Also add it to VariableSymbol and
    * MethodSymbol, to represent the type (or the return type) of these symbols.
    * Note that your Identifier tree node has now potentially a lot of redundant
    * information. A good idea is to override the setType and getType methods in
    * Identifier so that setting a type is impossible (it should always be done
    * through a symbol), and getting a type returns a result which depends on what
    * the identifier represents (what its symbol is). */
  trait Typed {
    self =>

    private var _tpe: Type = TUntyped

    def setType(tpe: Type): self.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }
}
