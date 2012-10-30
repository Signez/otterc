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
    override def isSubTypeOf(tpe: Type): Boolean = /* ... */
    override def toString = "[untyped]"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = /* ... */
    override def toString = "int"
  }

  /* ... */

  // It's convenient to reference classes by their symbol, rather than by their name, to avoid doing the lookup every time.
  case class TObject(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = /* ... */
    override def toString = /* ... */
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
