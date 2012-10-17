package toolc
package parser

object Trees {
  sealed trait Tree extends Positional

  case class Program(main: MainObject, classes: List[ClassDecl]) extends Tree
  case class MainObject(id: Identifier, stat: StatTree) extends Tree
  case class ClassDecl(id: Identifier, extendz: Option[Identifier], 
                              variables: List[VarDecl], methods: List[MethodDecl]) extends Tree
  case class VarDecl(id: Identifier, theType: TypeTree)
  case class MethodDecl(id: Identifier, arguments: List[VarDecl], returnType: TypeTree, 
                        variables: List[VarDecl], statements: List[StatTree], returnExpr: ExprTree) extends Tree
  

  sealed trait TypeTree extends Tree

  case class IntType() extends TypeTree        // Int
  case class BoolType() extends TypeTree       // Bool
  case class StringType() extends TypeTree     // String
  case class IntArrayType() extends TypeTree   // Int[]

  sealed trait StatTree extends Tree

  case class Block(stats: List[StatTree]) extends StatTree                                        // { stats; }
  case class If(condition: ExprTree, then: StatTree, elze: Option[StatTree]) extends StatTree     // if(condition) then; elze;
  case class While(condition: ExprTree, loop: StatTree) extends StatTree                          // while(condition) loop;
  case class PrintLn(expr: ExprTree) extends StatTree                                             // println(expr);
  case class Assignment(id: Identifier, expr: ExprTree) extends StatTree                          // id = expr;
  case class IndexAssignment(id: Identifier, index: ExprTree, expr: ExprTree) extends StatTree    // id[index] = expr;

  sealed trait ExprTree extends Tree

  case class Plus(lhs: ExprTree, rhs: ExprTree) extends ExprTree               // lhs + rhs
  case class Minus(lhs: ExprTree, rhs: ExprTree) extends ExprTree              // lhs - rhs
  case class Multiply(lhs: ExprTree, rhs: ExprTree) extends ExprTree           // lhs * rhs
  case class Divide(lhs: ExprTree, rhs: ExprTree) extends ExprTree             // lhs / rhs
  case class Or(lhs: ExprTree, rhs: ExprTree) extends ExprTree                 // lhs || rhs
  case class And(lhs: ExprTree, rhs: ExprTree) extends ExprTree                // lhs && rhs
  case class Equals(lhs: ExprTree, rhs: ExprTree) extends ExprTree             // lhs == rhs
  case class LesserThan(lhs: ExprTree, rhs: ExprTree) extends ExprTree        // lhs < rhs
  case class Index(lhs: ExprTree, rhs: ExprTree) extends ExprTree              // lhs[rhs]
  case class Length(expr: ExprTree) extends ExprTree                           // expr.length
  case class Not(expr: ExprTree) extends ExprTree                              // !expr
  case class MethodCall(objectId: ExprTree, methodId: Identifier, 
		  				expressions: List[ExprTree]) extends ExprTree          // objectId.methodId(expressions...)
  
  case class IntegerLiteral(value: Int) extends ExprTree                       // value (int)
  case class StringLiteral(value: String) extends ExprTree                     // "value"
  case class BooleanLiteral(value: Boolean) extends ExprTree                   // value (true or false)
  
  case class Variable(id: Identifier) extends ExprTree                         // id
  case class NewArray(length: ExprTree) extends ExprTree                       // new Int[length]
  case class NewObject(objectId: Identifier) extends ExprTree                  // new objectId()
  
  case class ThisObject() extends ExprTree                                     // this
  
  case class Identifier(value: String) extends TypeTree with ExprTree // special case :)
}