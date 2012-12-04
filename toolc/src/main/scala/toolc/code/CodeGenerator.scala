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
	    case TObject(classSymbol) => "L" + classSymbol.name + ";"
	    case _ => sys.error("Can't generate signature for type " + t) // TAny, TUntyped, TError
	  }
	}
  
    def generateMethodSignature(args: List[ExprTree], returnType: Type): String =  {
      "(" + (for(arg <- args) yield { getTypeSignature(arg.getType) }).mkString("") + ")" + getTypeSignature(returnType)
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
          (argument, index) <- method.arguments.zipWithIndex
        } yield (argument.getSymbol -> (index + 1))).toMap

      def evalExpr(expr: ExprTree): Unit = {
        expr match {
          // lhs + rhs
          case Plus(lhs, rhs) =>
            (lhs.getType, rhs.getType) match {
              case (TInt, TInt) =>
	            evalExpr(lhs); evalExpr(rhs)
	            ch << IADD
	          
              case lr @ _ =>
                ch << DefaultNew("java/lang/StringBuilder")
                evalExpr(lhs)
                if(lr._1 == TInt) {
                  ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
                } else {
                  ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
                }
                evalExpr(rhs)
                if(lr._2 == TInt) {
                  ch << InvokeVirtual("java/lang/StringBuilder", "append", "(I)Ljava/lang/StringBuilder;")
                } else {
                  ch << InvokeVirtual("java/lang/StringBuilder", "append", "(Ljava/lang/String;)Ljava/lang/StringBuilder;")
                }
                ch << InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
            }
            
          // lhs - rhs
          case Minus(lhs, rhs) =>     
            evalExpr(lhs); evalExpr(rhs)
            ch << ISUB         
            
          // lhs * rhs
          case Multiply(lhs, rhs) =>   
            evalExpr(lhs); evalExpr(rhs)
            ch << IMUL      
            
          // lhs / rhs
          case Divide(lhs, rhs) => 
            evalExpr(lhs); evalExpr(rhs)
            ch << IDIV      
            
          // lhs || rhs
          case Or(lhs, rhs) =>
            val elseLabel = ch.getFreshLabel("elseOr")
            val trueLabel = ch.getFreshLabel("trueOr")
            val endLabel = ch.getFreshLabel("endOr")
            
            evalExpr(lhs)
            ch << IfNe(trueLabel) 
            evalExpr(rhs)
            ch << IfNe(trueLabel) 
            ch << Ldc(0)
            ch << Goto(endLabel)
            
            ch << Label(trueLabel)
            ch << Ldc(1)
            
            ch << Label(endLabel)
            
          // lhs && rhs
          case And(lhs, rhs) =>
            val falseLabel = ch.getFreshLabel("falseAnd")
            val endLabel = ch.getFreshLabel("endAnd")
            
            evalExpr(lhs)
            ch << IfEq(falseLabel) 
            evalExpr(rhs)
            ch << IfEq(falseLabel) 
            ch << Ldc(1)
            ch << Goto(endLabel)
            
            ch << Label(falseLabel)
            ch << Ldc(0)
            
            ch << Label(endLabel)
            
          // lhs == rhs
          case Equals(lhs, rhs) =>
            val trueLabel = ch.getFreshLabel("trueEqual")
            val endLabel = ch.getFreshLabel("endEqual")
            evalExpr(lhs)
            evalExpr(rhs)
                
            (lhs.getType, rhs.getType) match {	
              case (TInt, TInt) | (TBoolean, TBoolean) => 
	            ch << If_ICmpEq(trueLabel)
	            
              case _ =>
                ch << If_ACmpEq(trueLabel)
            }
            ch << Ldc(0)
            ch << Goto(endLabel)
            
            ch << Label(trueLabel)
            ch << Ldc(1)
	            
            ch << Label(endLabel)
            
          // lhs < rhs
          case LesserThan(lhs, rhs) =>
            val trueLabel = ch.getFreshLabel("trueLT")
            val endLabel = ch.getFreshLabel("endLT")
            
            evalExpr(lhs)
            evalExpr(rhs)
            ch << If_ICmpLt(trueLabel)
            ch << Ldc(0)
            ch << Goto(endLabel)
            
            ch << Label(trueLabel)
            ch << Ldc(1)
            
            ch << Label(endLabel)
            
            
          // lhs[rhs]
          case idx @ Index(lhs, rhs) =>
            evalExpr(lhs)
            evalExpr(rhs)
            ch << IALOAD
            
          // expr.length
          case Length(expr) =>
            evalExpr(expr)
            ch << ARRAYLENGTH
            
          // !expr
          case Not(expr) =>
            val falseLabel = ch.getFreshLabel("falseNot")
            val endLabel = ch.getFreshLabel("endNot")
            
            evalExpr(expr)
            ch << IfNe(falseLabel)
            ch << Ldc(1)
            ch << Goto(endLabel)
            
            ch << Label(falseLabel)
            ch << Ldc(0)
            
            ch << Label(endLabel)
            
          // objectId.methodId(expressions...)
          case MethodCall(objectId, methodId, expressions) =>
            evalExpr(objectId)
            for(arg <- expressions) {
              evalExpr(arg)
            }
            objectId.getType match {
              case TObject(cs) => {
                val returnType = methodId.getSymbol.asInstanceOf[MethodSymbol].getType
                ch << InvokeVirtual(cs.name, methodId.value, generateMethodSignature(expressions, returnType))
              }
              case _ => sys.error("Trying to call a method on a non-object in the generating step.")
            }
            
          // value (int)
          case IntegerLiteral(value: Int) =>
            ch << Ldc(value)
            
          // "value"
          case StringLiteral(value: String) =>
            ch << Ldc(value)
            
          // value (true or false)
          case BooleanLiteral(value: Boolean) =>
            if(value)
              ch << Ldc(1)
            else
              ch << Ldc(0)
            
          // new Int[length]
          case toolc.parser.Trees.NewArray(length: ExprTree) =>
            evalExpr(length)
            // Beware: 10 is a constant that should not be replaced.
            //         Strings can only be used there with objects ; 
            //         not with primitive types.
            ch << AbstractByteCodes.NewArray(10) // 10 = Integer
            
          // new objectId()
          case NewObject(objectId: Identifier) =>
            ch << DefaultNew(objectId.getSymbol.name)
            
          // this
          case ThisObject() =>
            // "this" should not be called in main, this is checked before
            ch << ArgLoad(0)
            
          // id (special case :)
          case id @ Identifier(value) =>
            // Has to be correct (verified in Analyzer)
            val vs = id.getSymbol.asInstanceOf[VariableSymbol];
            val argIndex = paramMapping.get(vs)
            argIndex match {
              case Some(idx) => ch << ArgLoad(idx)
              case None => {
                // Passed the Analyzer : double-check is useless
                val maybeIdx = varMapping.get(vs)
                
                maybeIdx match {
                  case Some(idx) => vs.getType match { 
	                  case TInt => ch << ILoad(idx)
	                  case TBoolean => ch << ILoad(idx)
	                  case TIntArray => ch << ALoad(idx)
	                  case TString => ch << ALoad(idx)
	                  case TObject(_) => ch << ALoad(idx)
                  
	                  // We don't do anything for TAny and TError that shouldn't appear at this step
	                  case _ => sys.error("That should not happen.")
                  }
                  case None => 
                    ch << ArgLoad(0) // (getting from this)
                  	ch << GetField(classname, value, getTypeSignature(vs.getType))
                }
              }
            }
        }
      }
      
      def evalStat(stat: StatTree): Unit = {
        stat match {
          // TODO: Add opcodes to ch for every statements
          case If(condition, then, elze) => {
            val elseLabel = ch.getFreshLabel("elseIf")
            val endLabel = ch.getFreshLabel("endIf")
            
            evalExpr(condition)
            ch << IfEq(elseLabel)
            evalStat(then)
            ch << Goto(endLabel) << Label(elseLabel)
            elze match {
              case Some(e) => evalStat(e)
              case None =>
            }
            ch << Label(endLabel)
          }
          case While(condition, loop) => {
            val loopLabel = ch.getFreshLabel("loopWhile")
            val endLabel = ch.getFreshLabel("endWhile")
            
            evalExpr(condition)
            ch << IfEq(endLabel)
            
            ch << Label(loopLabel)
            evalStat(loop)
            evalExpr(condition)
            ch << IfEq(endLabel)
            ch << Goto(loopLabel)
            
            ch << Label(endLabel)
          }
          case Assignment(id, expr) => {
            id.getSymbol match {
              case vs @ VariableSymbol(value) =>
                
                val argIndex = paramMapping.get(vs)
                argIndex match {
                	case Some(idx) => 
                	   evalExpr(expr)
                	   ch << IStore(idx)
                	case None => 
		                	vs.parentSymbol match {
		                  case cs @ ClassSymbol(_) =>
		                    ch << ArgLoad(0)
		                    evalExpr(expr)
		                    ch << PutField(classname, id.value, getTypeSignature(id.getType))
		                  case ms @ MethodSymbol(_,_) => 
		                    vs.getType match {
		                      case TInt =>
		                        evalExpr(expr)
		                      	ch << IStore(varMapping(vs))
		                      case TBoolean =>
		                        evalExpr(expr)
		                        ch << IStore(varMapping(vs))
		                      case TString =>
		                        evalExpr(expr)
		                        ch << AStore(varMapping(vs))
		                      case TIntArray =>
		                        evalExpr(expr)
		                        ch << AStore(varMapping(vs))
		                      case TObject(_) =>
		                        evalExpr(expr)
		                        ch << AStore(varMapping(vs))
		                      case _ =>
		                    }
		
		                  case _ =>
		                }
                }
              case _ =>  
            }
          }
          case IndexAssignment(id, index, expr) => {
            id.getSymbol match {
              case vs @ VariableSymbol(_) =>
                vs.parentSymbol match {
                  case cs @ ClassSymbol(_) =>
                    ch << ArgLoad(0)
                    ch << GetField(classname, id.value, getTypeSignature(id.getType))		//"[I"
                    evalExpr(index)
                    evalExpr(expr)
                    ch << IASTORE
                  case ms @ MethodSymbol(_,_) =>
                    evalExpr(id)
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
              case TBoolean | TInt =>
                ch << InvokeVirtual("java/io/PrintStream", "println", "(I)V")
              case TString =>
                ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
              case _ => 
            }
          }
          case Block(statements) => {
            for(stat <- statements) {
              evalStat(stat)
            }
          }
        }
      }
      
      for(variable <- method.variables) {
        val varSymbol = variable.getSymbol
        varSymbol.getType match {
          case TInt =>
            ch << Ldc(0)
          	ch << IStore(varMapping(varSymbol))
          case TBoolean =>
            ch << Ldc(0)
            ch << IStore(varMapping(varSymbol))
          case TString =>
            ch << Ldc(0)
            ch << AStore(varMapping(varSymbol))
		  case TIntArray =>
            ch << Ldc(0)
            ch << AbstractByteCodes.NewArray(10)
            ch << AStore(varMapping(varSymbol))
		  case TObject(_) =>
            ch << Ldc(0)
            ch << AStore(varMapping(varSymbol))
          case _ =>
        }
      }
//      for(arguments <- method.arguments) {
//        val varSymbol = arguments.getSymbol
//        varSymbol.getType match {
//          case TInt =>
//            ch << Ldc(0)
//          	ch << IStore(varMapping(varSymbol))
//          case TBoolean =>
//            ch << Ldc(0)
//            ch << IStore(varMapping(varSymbol))
//          case _ =>
//        }
//      }
      
      for(stat <- method.statements) {
        evalStat(stat)
	  }
      
      evalExpr(method.returnExpr)
      method.getSymbol.getType match {
        case TInt => ch << IRETURN
        case TBoolean => ch << IRETURN
        case TIntArray => ch << ARETURN
        case TString => ch << ARETURN
        case TObject(_) => ch << ARETURN
        case _ => ch << POP << RETURN
      }
	  ch.freeze
	}
    
  def generateMainClassFile(srcFileName: String, gs: GlobalScope, mainObject: MainObject, dir: String) {
    val classFile = new ClassFile(mainObject.getSymbol.name, None)
    classFile.addDefaultConstructor
    classFile.setSourceFile("")
    val mainMethodHandler = classFile.addMainMethod
    val mainMethodDecl = new MethodDecl(new Identifier("main"), List(), new IntType(), List(), List(mainObject.stat), new IntegerLiteral(0))
    mainMethodDecl.setSymbol(new MethodSymbol("main", mainObject.getSymbol));
    mainMethodDecl.getSymbol.setType(TUntyped);
    addOpCode(mainMethodDecl, mainMethodHandler, gs, mainObject.id.value)
    classFile.writeToFile(dir + mainObject.getSymbol.name + ".class")
  }
  
  
  /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
  def generateClassFile(srcFileName: String, gs: GlobalScope, ct: ClassDecl, dir: String): Unit = {
    
    val classFile = 
	  ct.extendz match {
	    case Some(parent) => new ClassFile(ct.getSymbol.name, Some(parent.value))
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
