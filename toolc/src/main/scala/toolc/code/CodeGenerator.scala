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
  
	def getTypeSignature(t: Type): String = {
	  t match {
	    case TInt => "I"
	    case TString => "Ljava/lang/String;"
	    case TBoolean => "Z"
	    case TIntArray => "[I"
	    case TObject(classSymbol) => "L/" + classSymbol.name + ";"
	    case _ => sys.error("Can't generate signature for type " + t) // TAny, TUntyped, TError
	  }
	}
    
    def addOpCode(method: MethodDecl, mHandler: MethodHandler, gs: GlobalScope, classname: String): Unit = {
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

      def evalExpr(expr: ExprTree): Unit = {
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
      
      def evalStat(stat: StatTree): Unit = {
        stat match {
          // TODO: Add opcodes to ch for every statements
          case If(condition, then, elze) => {
            val elseLabel = ch.getFreshLabel("elseIf")
            val endLabel = ch.getFreshLabel("endIf")
            
            evalExpr(condition)
            ch << IfNull(elseLabel)
            evalStat(then)
            ch << Goto(endLabel) << Label(elseLabel)
            elze match {
              case Some(e) => evalStat(e)
              case None =>
            }
            ch << Label(endLabel)
          }
          case Assignment(id, expr) => {
            evalExpr(expr)
            id.getSymbol match {
              case vs @ VariableSymbol(_) =>
                vs.parentSymbol match {
                  case cs @ ClassSymbol(_) =>
                    ch << PutField(classname, id.value, getTypeSignature(id.getType))
                  case ms @ MethodSymbol(_,_) => 
                    vs.getType match {
                      case TInt =>
                        //TODO: IStore, LStore, DStore etc
                      	//TODO: store Param with ArgLoad
                      	ch << IStore(varMapping(vs))
                      case _ =>
                    }

                  case _ =>
                }
              case _ =>  
            }
          }
          case IndexAssignment(id, index, expr) => {
            evalExpr(expr)
            id.getSymbol match {
              case vs @ VariableSymbol(_) =>
                vs.parentSymbol match {
                  case cs @ ClassSymbol(_) =>
                    ch << GetField(classname, id.value, getTypeSignature(id.getType))		//"[I"
                    evalExpr(index)
                    evalExpr(expr)
                    ch << IASTORE  
                  case ms @ MethodSymbol(_,_) =>
                    ch << ALoad(varMapping(vs))
                    evalExpr(index)
                    evalExpr(expr)
                    ch << IASTORE  
                  case _ =>
                }
              case _ =>
            }
          }
          case PrintLn(expr) => {
            ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
            evalExpr(expr)
            expr.getType match {
              case TBoolean =>
                
              case TInt =>
                
              case TString =>
                //TODO: Remove this "hello world"
                ch << Ldc("hello World")
                ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
              case _ => 
            }
        	ch << RETURN
          }
          case Block(statements) => {
            for(stat <- method.statements) {
              evalStat(stat)
            }
          }
        }
      }
      
      for(stat <- method.statements) {
        evalStat(stat)
	  }
	  ch.print
	  ch.freeze
	}
    
  def generateMainClassFile(srcFileName: String, gs: GlobalScope, mainObject: MainObject, dir: String) {
    val classFile = new ClassFile(mainObject.getSymbol.name, None)
    classFile.addDefaultConstructor
    classFile.setSourceFile("")
    val mainMethodHandler = classFile.addMainMethod
    val mainMethodDecl = new MethodDecl(new Identifier("main"), List(), new IntType(), List(), List(mainObject.stat), new IntegerLiteral(0))
    addOpCode(mainMethodDecl, mainMethodHandler, gs, mainObject.id.value)
    classFile.writeToFile(dir + mainObject.getSymbol.name + ".class")
  }
  
  
  /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
  def generateClassFile(srcFileName: String, gs: GlobalScope, ct: ClassDecl, dir: String): Unit = {
    
    val classFile = 
	  ct.extendz match {
	    case Some(parent) => new ClassFile(ct.getSymbol.name, Some(parent.getSymbol.name))
	    case _ => new ClassFile(ct.id.value, None)
	  }
    
    classFile.addDefaultConstructor
    
    //Source File from which the class file was generated 
    classFile.setSourceFile("")
    
    //add field of class
    for (varDecl <- ct.variables) {
      classFile.addField(getTypeSignature(varDecl.getSymbol.getType), varDecl.id.value)
    }
    
    //create Op Code of methods
    for (methodDecl <- ct.methods) {
      val returnTypeSig = getTypeSignature(methodDecl.getSymbol.getType)
      val methodName = methodDecl.getSymbol.name
      val paramTypSig = methodDecl.arguments.flatMap(arg=>getTypeSignature(arg.getSymbol.getType)).mkString
      val methodHandler: MethodHandler = classFile.addMethod(returnTypeSig, methodName, paramTypSig)
      addOpCode(methodDecl, methodHandler, gs, ct.id.value)
    }
    
    classFile.writeToFile(dir + ct.getSymbol.name + ".class")
  }
}
