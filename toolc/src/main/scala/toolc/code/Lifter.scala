package toolc
package code

import scala.collection.immutable.HashMap

trait Lifter {
  self: Reporter =>

  import parser.Trees._
  import analyzer.Symbols._
  import analyzer.Types._
  
  object ClosureID {
    private var c: Int = 1

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }

  def generateType(typeT: TypeTree): Type = {
      typeT match {
        case IntType() => TInt
        case BoolType() => TBoolean
        case StringType() => TString
        case IntArrayType() => TIntArray
        case id @ Identifier(_) => TObject(id.getSymbol.asInstanceOf[ClassSymbol])
        case FuncType(args, returnType) => TFunction((args).map(el => generateType(el)), generateType(returnType)) 
        case UnitType() => TUnit
        case _ =>
          sys.error("Unexpected Type discovered! (" + TreePrinter(true)(typeT) + ")")
          null
      }
    }
  
  def liftTree(prog: Program, gs: GlobalScope) {
    
    var boxingClasses = new HashMap[TypeTree, ClassDecl]
    var addedClasses : List[ClassDecl] = Nil
    
    def getTypeTree(tpe: Type): TypeTree = tpe match {
      case TInt => new IntType
      case TIntArray => new IntArrayType
      case TString => new StringType
      case TObject(cs) => 
        val objId = new Identifier(cs.name)
        objId.setSymbol(cs)
        objId
      case TBoolean => new BoolType
      case TUnit => new UnitType
      case TFunction(in, out) => new FuncType(in.map(plop => getTypeTree(plop)), getTypeTree(out))
      case _ => fatalError("This should not happen.")
    }
    
    def createBoxingClasses(contentType: TypeTree): ClassDecl = {
      boxingClasses.get(contentType) match {
        case Some(boxingClass) => return boxingClass
        case None =>
          val contentId = new Identifier("content")
	      val contentVar = new VarDecl(contentId, contentType);
	      val getMethod = new MethodDecl(new Identifier("get"), Nil, 
	    		  						 contentType, Nil, Nil, Some(contentId));
	      val valueId = new Identifier("value")
	      val setMethod = new MethodDecl(new Identifier("set"), 
	    		  						 List(new VarDecl(valueId, contentType)), 
	    		  						 new UnitType, Nil, 
	    		  						 List(new Assignment(contentId, valueId)), 
	    		  						 None);
	      
	      val clazz = new ClassDecl(Identifier("Boxed" + TreePrinter.apply(true)(contentType)), 
	                                None, List(contentVar), List(getMethod, setMethod));
	      
	      val classSymbol = new ClassSymbol(clazz.id.value);
	
	      classSymbol.parent = None
	
	      for (method <- clazz.methods) {
	        val methodSymbol = new MethodSymbol(method.id.value, classSymbol, None);
	
	        for (param <- method.arguments) {
	          val variableSymbol = new VariableSymbol(param.id.value);
	          variableSymbol.parentSymbol = methodSymbol
	          variableSymbol.setType(generateType(param.theType))
	          
	          param.setSymbol(variableSymbol);
	          param.id.setSymbol(variableSymbol);
	
	          methodSymbol.params += param.id.value -> variableSymbol;
	          methodSymbol.argList ::= variableSymbol;
	        }
	        
	        methodSymbol.argList = methodSymbol.argList.reverse
	
	        for (member <- method.variables) {
	          val memberSymbol = new VariableSymbol(member.id.value);
	          memberSymbol.parentSymbol = methodSymbol
	          memberSymbol.setType(generateType(member.theType))
	
	          member.setSymbol(memberSymbol);
	          member.id.setSymbol(memberSymbol);
	
	          methodSymbol.members += member.id.value -> memberSymbol;
	        }
	
	        method.setSymbol(methodSymbol);
	        method.id.setSymbol(methodSymbol);
	        
	        method.getSymbol.setType(generateType(method.returnType))
	
	        classSymbol.methods += method.id.value -> methodSymbol
	      }
	
	      for(variable <- clazz.variables) {
	        val variableSymbol = new VariableSymbol(variable.id.value);
	        variableSymbol.parentSymbol = classSymbol
	        variableSymbol.setType(generateType(contentType))
	        
	        variable.setSymbol(variableSymbol);
	        variable.id.setSymbol(variableSymbol);
	
	        classSymbol.members += variable.id.value -> variableSymbol;
	      }
	      
	      clazz.setSymbol(classSymbol);
	      clazz.id.setSymbol(classSymbol);
	
	      gs.classes += clazz.id.value -> classSymbol;
	      
	      boxingClasses += contentType -> clazz
	      
	      addedClasses ::= clazz
	      
	      return clazz
      }
    }
    
    def replaceInStat(stat: StatTree) : StatTree = {
      stat match {
        case assign @ Assignment(id, expr) =>
          val sym = id.getSymbol
          if(sym.isInstanceOf[VariableSymbol]) {
            val vs = sym.asInstanceOf[VariableSymbol]
            if(vs.needBoxing) {
              // Unit call = If.
              return new If(new MethodCall(id, new Identifier("set"), List(replaceInExpr(expr))), new Block(Nil), None);
            } else {
              return new Assignment(id, replaceInExpr(expr));
            }
          } 
          return stat
        case _ => return stat
      }
    }
    
    def replaceInVarDeclList(list: List[VarDecl]) : List[VarDecl] = {      
      list.map(vd => {
        val nvd = new VarDecl(vd.id, createBoxingClasses(vd.theType).id)
        nvd.setSymbol(vd.getSymbol)
        nvd.getSymbol.setType(generateType(createBoxingClasses(vd.theType).id))
        nvd
      })
    }
    
    def replaceInExpr(expr: ExprTree) : ExprTree = {
      expr match {
        case id @ Identifier(_) =>
          val sym = id.getSymbol
          if(sym.isInstanceOf[VariableSymbol]) {
            val vs = sym.asInstanceOf[VariableSymbol]
            if(vs.needBoxing) {
              return new MethodCall(id, new Identifier("get"), List())
            }
          }
          return expr
        
        case fun @ FuncExpr(arguments, variables, statements, returnExpr) =>
          
          var _args = replaceInVarDeclList(arguments)
          var _vars = replaceInVarDeclList(variables)
          var _stats = statements.map(stat => replaceInStat(stat));
          var _expr = if(returnExpr.isDefined) Some(replaceInExpr(returnExpr.get)); else None;
          
	      val applyMethod = new MethodDecl(new Identifier("apply"), _args, 
	    		  						   getTypeTree(fun.getType), _vars, _stats, _expr);
          
	      var contextVars = List[VarDecl]()
	      var contextSetting = List[Assignment]()
	      var contextSettingArgs = List[VarDecl]()
	      
          for((ctxName, ctxSymbol) <- fun.getSymbol.contextualParams) {
            val oneVar = new VarDecl(new Identifier(ctxName), getTypeTree(ctxSymbol.getType));
            val underVar = new VarDecl(new Identifier("_" + ctxName), getTypeTree(ctxSymbol.getType));
            contextVars ::= oneVar
            contextSettingArgs ::= underVar
            contextSetting ::= new Assignment(new Identifier(ctxName), new Identifier("_" + ctxName))
          }
	      
	      contextVars.reverse
	      
	      val setContextMethod = new MethodDecl(new Identifier("setContext"), contextSettingArgs, new UnitType,
	    		  								Nil, contextSetting, None);
	      
	      val clazz = new ClassDecl(Identifier("Closure" + ClosureID.next), None, 
	                                contextVars, 
	                                List(applyMethod, setContextMethod));
	      
	      val classSymbol = new ClassSymbol(clazz.id.value);
	
	      classSymbol.parent = None
	
	      for (method <- clazz.methods) {
	        val methodSymbol = new MethodSymbol(method.id.value, classSymbol, None);
	
	        for (param <- method.arguments) {
	          val variableSymbol = new VariableSymbol(param.id.value);
	          variableSymbol.parentSymbol = methodSymbol
	          variableSymbol.setType(generateType(param.theType))
	          
	          param.setSymbol(variableSymbol);
	          param.id.setSymbol(variableSymbol);
	
	          methodSymbol.params += param.id.value -> variableSymbol;
	          methodSymbol.argList ::= variableSymbol;
	        }
	        
	        methodSymbol.argList = methodSymbol.argList.reverse
	
	        for (member <- method.variables) {
	          val memberSymbol = new VariableSymbol(member.id.value);
	          memberSymbol.parentSymbol = methodSymbol
	          memberSymbol.setType(generateType(member.theType))
	
	          member.setSymbol(memberSymbol);
	          member.id.setSymbol(memberSymbol);
	
	          methodSymbol.members += member.id.value -> memberSymbol;
	        }
	
	        method.setSymbol(methodSymbol);
	        method.id.setSymbol(methodSymbol);
	        
	        method.getSymbol.setType(generateType(method.returnType))
	
	        classSymbol.methods += method.id.value -> methodSymbol
	      }
	
	      for(variable <- clazz.variables) {
	        val variableSymbol = new VariableSymbol(variable.id.value);
	        variableSymbol.parentSymbol = classSymbol
	        variableSymbol.setType(generateType(variable.theType))
	        
	        variable.setSymbol(variableSymbol);
	        variable.id.setSymbol(variableSymbol);
	
	        classSymbol.members += variable.id.value -> variableSymbol;
	      }
	      
	      clazz.setSymbol(classSymbol);
	      clazz.id.setSymbol(classSymbol);
	
	      gs.classes += clazz.id.value -> classSymbol;
	      
	      addedClasses ::= clazz
	      
	      return new MethodCall(new NewObject(clazz.id), setContextMethod.id, 
	    		  			   fun.getSymbol.contextualParams.toList.map(mixed => {
	    		  			     val ident = new Identifier(mixed._1);
	    		  			     ident.setSymbol(mixed._2);
	    		  			     return ident;
	    		  			   }));
          
        case Plus(lhs, rhs) => new Plus(replaceInExpr(lhs), replaceInExpr(rhs))
        case Minus(lhs, rhs) => new Minus(replaceInExpr(lhs), replaceInExpr(rhs))
        case Multiply(lhs, rhs) => new Multiply(replaceInExpr(lhs), replaceInExpr(rhs))
        case Divide(lhs, rhs) => new Divide(replaceInExpr(lhs), replaceInExpr(rhs))
        case Or(lhs, rhs) => new Or(replaceInExpr(lhs), replaceInExpr(rhs))
        case And(lhs, rhs) => new And(replaceInExpr(lhs), replaceInExpr(rhs))
        case Equals(lhs, rhs) => new Equals(replaceInExpr(lhs), replaceInExpr(rhs))
        case LesserThan(lhs, rhs) => new LesserThan(replaceInExpr(lhs), replaceInExpr(rhs))
        case Index(lhs, rhs) => new Index(replaceInExpr(lhs), replaceInExpr(rhs))
        case Length(expr) => new Length(replaceInExpr(expr))
        case Not(expr) => new Not(replaceInExpr(expr))
        case MethodCall(objectId, methodId, expressions) =>
          new MethodCall(replaceInExpr(objectId), methodId, expressions.map(expr => replaceInExpr(expr)))
          
        case i @ IntegerLiteral(value) => i
        case s @ StringLiteral(value) => s
        case b @ BooleanLiteral(value) => b
          
        case NewArray(length)  => new NewArray(replaceInExpr(length))
        case o @ NewObject(objectId)  => o
          
        case thisO @ ThisObject() => thisO
        
        case FuncCall(function, expressions) => new FuncCall(replaceInExpr(function), expressions.map(exp => replaceInExpr(exp)))
      }
    }
    
    def replaceInClass(clazz: ClassDecl) : ClassDecl = {
      for(method <- clazz.methods) {
        method.statements = method.statements.map(stat => replaceInStat(stat));
        
        for(localVar <- method.variables) {
          if(localVar.getSymbol.needBoxing) {
            localVar.theType = createBoxingClasses(localVar.theType).id
            localVar.getSymbol.setType(generateType(createBoxingClasses(localVar.theType).id))
          }
        }
        
        if(method.returnExpr.isDefined) {
          method.returnExpr = Some(replaceInExpr(method.returnExpr.get));
        }
      }
      clazz
    }
    
    prog.classes = prog.classes.map(clazz => replaceInClass(clazz))
    prog.classes = prog.classes ++ addedClasses
    prog.classes = prog.classes.reverse
  }
}