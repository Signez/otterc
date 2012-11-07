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
    /** Suggested inner function:
     *
     * Computes the type of an expression. If exp is not empty, checks that
     * the expression is a subtype of one in exp. If it's not, prints an
     * error message and returns the first element of exp. Returning a valid
     * type despite the error is a way to do error recovering: type checking
     * will continue, assuming the correct type was found. */
    def tcExpr(expr: ExprTree, exp: Type*): Type = {
      // make sure you attach symbols to method calls in the process!
    }

    /** for statements... */
    def tcStat(stat: StatTree): Unit = {
      /* ... */
    }

    /* ... */
  }
}
