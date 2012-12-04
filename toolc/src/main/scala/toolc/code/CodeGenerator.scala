package toolc
package code

trait CodeGenerator {
  self: Reporter =>

  import parser.Trees._
  import analyzer.Symbols._
  import analyzer.Types._
  import cafebabe._

  // Bytecodes
  import AbstractByteCodes._
  import ByteCodes._

  /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
  def generateClassFile(srcFileName: String, gs: GlobalScope, ct: ClassDecl, dir: String): Unit = {
    def getTypeSignature(t: Type): String = {
      t match {
        case TInt => "I"
        case TString => "Ljava/lang/String;"
        case TBoolean => "Z"
        case TIntArray => "[I"
        case TObject(classSymbol) => ""
        case _ => sys.error("Can't generate signature for type " + t) // TAny, TUntyped, TError
      }
    }
    
    def addOpCode(method: MethodDecl, mHandler: MethodHandler): Unit = {
      val ch: CodeHandler = mHandler.codeHandler

      //mapping var symbols of method to slot indice 
      val varMapping =
        (for {
          variable <- method.variables
        } yield (variable.getSymbol -> ch.getFreshVar)).toMap
      val paramMapping =
        (for {
          argument <- method.arguments
        } yield (argument.getSymbol -> ch.getFreshVar)).toMap
      val methodVarMapping = varMapping ++ paramMapping

      def evalExpr(expr: ExprTree, ch: CodeHandler): Unit = {
        expr match {
          // lhs + rhs
          case Plus(lhs, rhs) =>
            
          // lhs - rhs
          case Minus(lhs, rhs) =>              
            
          // lhs * rhs
          case Multiply(lhs, rhs) =>
            
          // lhs / rhs
          case Divide(lhs, rhs) =>
            
          // lhs || rhs
          case Or(lhs, rhs) =>
            
          // lhs && rhs
          case And(lhs, rhs) =>
            
          // lhs == rhs
          case Equals(lhs, rhs) =>
            
          // lhs < rhs
          case LesserThan(lhs, rhs) =>
            
          // lhs[rhs]
          case Index(lhs, rhs) =>
            
          // expr.length
          case Length(expr) =>
            
          // !expr
          case Not(expr) =>
            
          // objectId.methodId(expressions...)
          case MethodCall(objectId, methodId, expressions) =>
            
          // value (int)
          case IntegerLiteral(value: Int) =>
            
          // "value"
          case StringLiteral(value: String) =>
            
          // value (true or false)
          case BooleanLiteral(value: Boolean) =>
            
          // new Int[length]
          case toolc.parser.Trees.NewArray(length: ExprTree) =>
            
          // new objectId()
          case NewObject(objectId: Identifier) =>
            
          // this
          case ThisObject() =>
            
          // id (special case :)
          case Identifier(value) => 
        }
      }
      
      def evalStat(stat: StatTree, ch: CodeHandler): Unit = {
        stat match {
          // TODO: Add opcodes to ch for every statements
          case If(condition, then, elze) => {
            val elseLabel = ch.getFreshLabel("elseIf")
            val endLabel = ch.getFreshLabel("endIf")
            
            evalExpr(condition, ch)
            ch << IfNull(elseLabel)
            evalStat(then, ch)
            ch << Goto(endLabel) << Label(elseLabel)
            elze match {
              case Some(e) => evalStat(e, ch)
              case None =>
            }
            ch << Label(endLabel)
          }
          case Assignment(id, expr) => {
            evalExpr(expr, ch)
            id.getSymbol match {
              case vs @ VariableSymbol(_) =>
                vs.parentSymbol match {
                  case cs @ ClassSymbol(_) =>
                    ch << PutField(ct.getSymbol.name, id.value, getTypeSignature(id.getType))
                  case ms @ MethodSymbol(_,_) =>
                    ch << IStore(methodVarMapping(vs))
                  case _ =>
                }
              case _ =>  
            }
          }
          case PrintLn(expr) => {
            
          }
          case Block(statements) => {
            
          }
          case IndexAssignment(id, index, expr) => {
            
          }
        }
      }
      
      for(decl <- method.statements) {
        
      }
    }
    
    
    val classFile = 
	  ct.extendz match {
	    case Some(parent) => new ClassFile(ct.getSymbol.name, Some(parent.getSymbol.name))
	    case _ => new ClassFile(ct.id.value, None)
	  }
    
    //Source File from which the class file was generated 
    classFile.setSourceFile("")
    
    //add field of class
    for (varDecl <- ct.variables) {
      classFile.addField(getTypeSignature(varDecl.getSymbol.getType), varDecl.id.value)
    }
    
    //iterate through all methods
    for (methodDecl <- ct.methods) {
      val returnTypeSig = getTypeSignature(methodDecl.getSymbol.getType)
      val methodName = methodDecl.getSymbol.name
      val paramTypSig = methodDecl.arguments.map(arg=>getTypeSignature(arg.getSymbol.getType)).mkString
      val methodHandler: MethodHandler = classFile.addMethod(returnTypeSig, methodName, paramTypSig)
      addOpCode(methodDecl, methodHandler)
    }
    
    classFile.writeToFile("./" + ct.getSymbol.name + ".class")
  }
}
