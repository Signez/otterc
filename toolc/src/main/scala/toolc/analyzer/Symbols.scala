package toolc
package analyzer

import scala.collection.mutable.HashMap
import toolc.analyzer.Types.Typed

object Symbols {
  /** A trait for anything that refers to a symbol. */
  trait Symbolic[S <: Symbol] {
    self =>

    private var _sym: Option[S] = None

    def setSymbol(sym: S): self.type = {
      _sym = Some(sym)
      this
    }

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None => sys.error("Accessing undefined symbol.")
    }
  }

  /** Notice that creating a symbol will always automatically assign a unique integer id to it. */
  abstract class Symbol extends Positional {
    val id: Int = ID.next
    val name: String
  }

  private object ID {
    private var c: Int = 0

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }

  class GlobalScope {
    var mainClass: ClassSymbol = _
    var classes: HashMap[String,ClassSymbol] = new HashMap[String,ClassSymbol]

    def lookupClass(n: String): Option[ClassSymbol] = { 
	  classes.get(n)
    }
  }

  case class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = new HashMap[String, MethodSymbol]
    var members = new HashMap[String, VariableSymbol]

    def lookupMethod(n: String): Option[MethodSymbol] = {      
      if (parent.isDefined) {
		methods.get(n) match {
		  case ms @ Some(_) => ms
		  case None => parent.get.lookupMethod(n)   
	    }
	  } else {
	    methods.get(n)
	  }
    }
    
    def lookupVar(n: String): Option[VariableSymbol] = {
      if (parent.isDefined) {
		members.get(n) match {
		  case vs @ Some(_) => vs
		  case None => parent.get.lookupVar(n)   
	    }
	  } else {
	    members.get(n)
	  }
    }
  }
  

  case class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol with Typed {
    var params: HashMap[String, VariableSymbol] = new HashMap[String, VariableSymbol]
    var members: HashMap[String, VariableSymbol] = new HashMap[String, VariableSymbol]
    // should contain the same values as the params map, but in the right order.
    var argList: List[VariableSymbol] = Nil

    def lookupVar(n: String): Option[VariableSymbol] = {
      params.get(n) match {
        case vs @ Some(_) => vs
        case None => 
          members.get(n) match {
            case ms @ Some(_) => ms
            case None => classSymbol.lookupVar(n)
          }
      }   
    }
  }

  case class VariableSymbol(val name: String) extends Symbol with Typed {
    var parentSymbol: Symbol = null
  }
}
