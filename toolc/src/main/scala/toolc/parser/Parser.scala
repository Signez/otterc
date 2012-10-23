package toolc
package parser

import lexer.Lexer
import scala.io.Source

/** LL parser for the Tool grammar. */
trait Parser extends Lexer {
  self: Compiler =>

  import Trees._
  import lexer.Tokens._

  def parseSource: Tree = {
    readToken // initializes the parser by calling the method in the Lexer.
    val tree: Tree = parseGoal
    terminateIfErrors
    tree
  }

  /** Store the current token, as read from the lexer. */
  private var currentToken: Token = Token(BAD)

  def readToken: Unit = {
    /* update currentToken using nextToken in the Lexer. */
    currentToken = nextToken
  }

  /** 
   * ''Eats'' the expected token, or terminates with an error. 
   * 
   * Similar to skip in the course. 
   */
  private def eat(expectedTokenClass: TokenClass): Unit = {
    if(expectedTokenClass != currentToken.tokenClass) {
      expected(expectedTokenClass);
    }
    readToken;
  }

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenClass */
  private def expected(tokenClass: TokenClass, more: TokenClass*): Nothing = {
    fatalError("expected: " + (tokenClass::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
  }
  
  /**
   * Parse the "goal", the whole program.
   */
  private def parseGoal: Program = {
    val main = parseMainObject;
    val classes = parseClasses;
    
    return new Program(main, classes).setPos(main);
  }
  
  /**
   * Parse main object.
   */
  def parseMainObject: MainObject = {
    val initial = currentToken;
    
    // object id
    eat(OBJECT);
    val id = parseIdentifier;
    // { def main() : Unit = { stat
    eat(OBLOCK); eat(DEF); eat(MAIN); eat(OPAREN); eat(CPAREN); eat(COLON);
    eat(UNIT); eat(ASSIGN); eat(OBLOCK);
    val stat = parseStatement;
    // }}
    eat(CBLOCK); eat(CBLOCK);
    
    return new MainObject(id, stat).setPos(initial);
  }
  
  /**
   * Parse classes.
   */
  def parseClasses: List[ClassDecl] = {
    var classes : List[ClassDecl] = List();
    
    while(currentToken != EOF) {
      val initial = currentToken;
      
      eat(CLASS);
      val classId = parseIdentifier;
      var extendz : Option[Identifier] = None;
      if(currentToken == EXTENDS) {
        eat(EXTENDS);
        extendz = Some(parseIdentifier);
      }
      eat(OBLOCK);
      val variables = parseVariablesDecl;
      val methods = parseMethodsDecl;
      eat(CBLOCK);
      
      val classDecl = new ClassDecl(classId, extendz, variables, methods).setPos(initial);
      classes = classDecl :: classes;
    }
    
    return classes.reverse;
  }
  
  def parseVariablesDecl: List[VarDecl] = {
    var variables : List[VarDecl] = List();
    
    while(currentToken == VAR) {
      val initial = currentToken;
      
      eat(VAR);
      val nameId = parseIdentifier;
      eat(COLON);
	  val theType = parseType;
	  eat(SEMICOLON);
	  
	  val variable = new VarDecl(nameId, theType).setPos(initial);
	  
	  variables = variable :: variables;
    }
    
	return variables.reverse;
  }
  
  def parseType: TypeTree = {
    val initial = currentToken;
    currentToken.tokenClass match {
      case INT =>
        // Int[]
        readToken;
        if(currentToken == OBRACKET) {
          eat(OBRACKET);
          eat(CBRACKET);
          
          return new IntArrayType().setPos(initial);
          
        // Int
        } else {
          return new IntType().setPos(initial);
        }
        
      case STRING =>
        // String
        return new StringType().setPos(currentToken);
        
      case BOOL =>
        return new BoolType().setPos(currentToken);
        
      case IDCLASS =>
        return parseIdentifier;
        
      case _ =>
        expected(INT, STRING, BOOL, IDCLASS);
    }
  }
  
  /**
   * Parse method declarations.
   */
  def parseMethodsDecl: List[MethodDecl] = {
    var methods : List[MethodDecl] = List();
    
    // Looping while we find def keywords
    while(currentToken == DEF) {
        val initial = currentToken;
        
		eat(DEF);
		val methodId = parseIdentifier;
		eat(OPAREN);
		
		var arguments: List[VarDecl] = List();
		if(currentToken != CPAREN) {
		  while(currentToken != CPAREN) {
		    if(arguments.length > 0)
		      eat(COMMA);
		    
			val argId = parseIdentifier;
		    eat(COLON);
		    val firstVarType = parseType;
		  }
		}
		
		eat(CPAREN);
		eat(COLON);
		
		val returnType = parseType;
		
		eat(ASSIGN);
		eat(OBLOCK);
		
		val variables = parseVariablesDecl;
		
		var statements : List[StatTree] = List();
		while(currentToken != RETURN) {
		  val stat = parseStatement; 
		  statements = stat :: statements;
		}
		
		eat(RETURN);
		val returnExpr = parseExpression;
		
		val method = new MethodDecl(methodId, arguments, returnType, variables, statements, returnExpr).setPos(initial); 
    }
	
    return methods.reverse;
  }
  
  def parseStatement : StatTree = {
    
    currentToken.tokenClass match {
    case OBLOCK =>
      var list: List[StatTree] = List();
      readToken
      while (currentToken != CBLOCK || currentToken == EOF) {
        list = parseStatement :: list
      }
      readToken
      return new Block(list)
      
    // println ( expression )
    case PRINTLN =>
	  readToken
      eat(OPAREN)
      val expr : ExprTree = parseExpression
      eat(CPAREN)
	  eat(SEMICOLON)
      return new PrintLn(expr)
	  
    // | ident = expr
    case IDCLASS =>
      val ident : Identifier = parseIdentifier
      readToken
      if (currentToken == OBRACKET) {
	    readToken
	    val idx : ExprTree = parseExpression
	    eat(CBRACKET)
	    eat(ASSIGN)
	    val expr : ExprTree = parseExpression
	    eat(SEMICOLON)
	    return new IndexAssignment(ident, idx, expr)
      } else {
    	eat(ASSIGN)
	    val expr : ExprTree = parseExpression
	    eat(SEMICOLON)
	    return new Assignment(ident, expr)
      }
      
    // | if ( expr ) statmt (else statmt)?
    case IF =>
      readToken
      eat(OPAREN)
      val expr : ExprTree = parseExpression
      eat(CPAREN)
      val stat : StatTree = parseStatement
      var elseStat : Option[StatTree] =
        if (currentToken == ELSE) {
          readToken
          new Some[StatTree](parseStatement)
        } else null
      return new If(expr, stat, elseStat)
      
    // | while ( expr ) statmt
    case WHILE =>
       readToken
       eat(OPAREN)
       val expr : ExprTree = parseExpression
       eat(CPAREN)
       val stat : StatTree = parseStatement
       return new While(expr, stat)
       
    case _ =>
      expected(OBLOCK, PRINTLN, IDCLASS, IF, WHILE)
    }
  }
  
  /**
   * Parse an expression.
   */
  def parseExpression : ExprTree = {
    return parseOrExpr;
  }
  
  /**
   * Parse a || expression.
   * 
   * leftExpr || rightExpr
   */
  def parseOrExpr : ExprTree = {
    val leftExpr = parseAndExpr;
    //readToken;
    
    // leftExpr || rightExpr
    if(currentToken == OR) {
      eat(OR);
      val rightExpr = parseOrExpr;
      return new Or(leftExpr, rightExpr).setPos(leftExpr);
      
    } else {
      return leftExpr;
    }
  }
  
  /**
   * Parse a && expression.
   * 
   * leftExpr && rightExpr
   */
  def parseAndExpr : ExprTree = {
    val leftExpr = parseEqualsLesserThanExpr;
    //readToken;
    
    // leftExpr && rightExpr
    if(currentToken == AND) {
      eat(AND);
      val rightExpr = parseAndExpr;
      return new And(leftExpr, rightExpr).setPos(leftExpr);
      
    } else {
      return leftExpr;
    }
  }
  
  /**
   * Parse a == or a < expression.
   * 
   * leftExpr < rightExpr
   * leftExpr == rightExpr 
   */
  def parseEqualsLesserThanExpr : ExprTree = {    
    val leftExpr = parsePlusMinusExpr;
    //readToken;
    
    // leftExpr < rightExpr
    if(currentToken == LESS) {
      eat(LESS);
      val rightExpr = parseEqualsLesserThanExpr;
      return new LesserThan(leftExpr, rightExpr).setPos(leftExpr);
      
    // leftExpr == rightExpr
    } else if(currentToken == EQUALS) {
      eat(EQUALS);
      val rightExpr = parseEqualsLesserThanExpr;
      return new Equals(leftExpr, rightExpr).setPos(leftExpr);
      
    } else {
      return leftExpr;
    }
  }
  
  /**
   * Parse an term expression.
   * 
   * leftExpr + rightExpr
   * leftExpr - rightExpr
   */
  def parsePlusMinusExpr : ExprTree = {    
    val leftExpr = parseMultiplyDivideExpr;
    //readToken;
    
    // leftExpr + rightExpr
    if(currentToken == PLUS) {
      eat(PLUS);
      val rightExpr = parsePlusMinusExpr;
      return new Plus(leftExpr, rightExpr).setPos(leftExpr);
      
    // leftExpr - rightExpr
    } else if(currentToken == MINUS) {
      eat(MINUS);
      val rightExpr = parsePlusMinusExpr;
      return new Minus(leftExpr, rightExpr).setPos(leftExpr);
      
    } else {
      return leftExpr;
    }
  }
  
  /**
   * Parse a factor expression.
   * 
   * leftExpr * rightExpr
   * leftExpr / rightExpr
   */
  def parseMultiplyDivideExpr : ExprTree = {    
    val leftExpr = parseNotExpr;
    //readToken;
    
    // leftExpr * rightExpr
    if(currentToken == MUL) {
      eat(MUL);
      val rightExpr = parseMultiplyDivideExpr;
      return new Multiply(leftExpr, rightExpr).setPos(leftExpr);
      
    // leftExpr / rightExpr
    } else if(currentToken == DIV) {
      eat(DIV);
      val rightExpr = parseMultiplyDivideExpr;
      return new Divide(leftExpr, rightExpr).setPos(leftExpr);
      
      
    } else {
      return leftExpr;
    }
  }
  
  /**
   * Parse a unary ! expression.
   * 
   * !rightExpr
   */
  def parseNotExpr : ExprTree = {
    val initial = currentToken;
    //readToken;
    
    // !rightExpr
    if(currentToken == BANG) {
      eat(BANG);
      val rightExpr = parseNotExpr;
      return new Not(rightExpr).setPos(initial);
      
    } else {
      return parseDotExpr;
    }
  }
  
  /**
   * Parse a dot expression.
   * 
   * leftExpr.length
   * leftExpr.methodId
   */
  def parseDotExpr: ExprTree = {
    val leftExpr = parseIndexExpr;
    //readToken;
    
    // leftExpr.
    if(currentToken == DOT) {
      eat(DOT);
      
      // leftExpr.length
      if(currentToken == LENGTH) {
        eat(LENGTH);
        return new Length(leftExpr).setPos(leftExpr);
        
      // leftExpr.methodId(paramList)
      } else {
        val methodId = parseIdentifier;
        val paramList = parseParametersList;
        return new MethodCall(leftExpr, methodId, paramList).setPos(leftExpr);
      }
    } else {
      return leftExpr;
    } 
  }
  
  /**
   * Parse an indexed (access to an array) expression.
   * 
   * leftExpr[expr]
   */
  def parseIndexExpr: ExprTree = {
    val leftExpr = parseSimpleExpr;
    //readToken;
    
    // leftExpr[expr]
    if(currentToken == OBRACKET) {
      eat(OBRACKET);
      var index = parseExpression;
      eat(CBRACKET);
      
      return new Index(leftExpr, index).setPos(leftExpr);
    } else {
      return leftExpr;
    } 
  }
  
  /**
   * Parse a list of parameters.
   * 
   * (expr, expr, expr)
   */
  def parseParametersList: List[ExprTree] = {
    var list: List[ExprTree] = List();
    
    eat(OPAREN);
    if(currentToken != CPAREN) {
	  val expr = parseExpression;
	  list = expr :: list;
	    
	  while(currentToken == COMMA) {
	    eat(COMMA);
	      
	    val expr = parseExpression;
	    list = expr :: list;
	  }
    }
    
    eat(CPAREN);
    
    return list.reverse;
  }
  
  /**
   * Parse a "simple" expression, such as parenthesis, identifier, integer literal,
   * string literal, boolean literal, etc.
   */
  def parseSimpleExpr: ExprTree = {
    val initial = currentToken;
    
    currentToken.tokenClass match {
      // ( expr )
      case OPAREN => 
        eat(OPAREN);
	    val expr = parseExpression;
	    eat(CPAREN);
	    return expr;
	    
	  // identifier
      case IDCLASS =>
        return parseIdentifier;
        
      // integer literal
      case INTEGERLITERALCLASS =>
        return parseIntegerLiteral;
        
      // string literal
      case STRINGLITERALCLASS =>
        return parseStringLiteral;
        
      // boolean literals
      case TRUE =>
        val expr = BooleanLiteral(true).setPos(currentToken); 
        readToken;
        return expr;
      case FALSE =>
        val expr = BooleanLiteral(false).setPos(currentToken); 
        readToken;
        return expr;
        
      // this
      case THIS =>
        eat(THIS);
        return ThisObject().setPos(currentToken);
        
      case NEW =>
        eat(NEW);
        val id = parseIdentifier;
        
        // new Int[expr]
        if(id.value == "Int") {
          eat(OBRACKET);
          val expr = parseExpression;
          eat(CBRACKET);
          return new NewArray(expr).setPos(initial);
          
        // new id()
        } else {
          eat(OPAREN);
	      eat(CPAREN);
	      return new NewObject(id).setPos(initial);
        }
        
      case _ =>
        expected(OPAREN, IDCLASS, INTEGERLITERALCLASS, STRINGLITERALCLASS, TRUE, FALSE, THIS, NEW)
    }
  }

  /**
   * Parse an identifier.
   */
  private def parseIdentifier: Identifier = currentToken.info match {
    case ID(value) => { val ret = Identifier(value).setPos(currentToken); readToken; ret }
    case _ => expected(IDCLASS)
  }
  
  /**
   * Parse an integer literal.
   */
  private def parseIntegerLiteral: IntegerLiteral = currentToken.info match {
    case INTEGERLITERAL(value) => { val ret = IntegerLiteral(value).setPos(currentToken); readToken; ret }
    case _ => expected(INTEGERLITERALCLASS)
  }
  
  /**
   * Parse a string literal.
   */
  private def parseStringLiteral: StringLiteral = currentToken.info match {
    case STRINGLITERAL(value) => { val ret = StringLiteral(value).setPos(currentToken); readToken; ret }
    case _ => expected(STRINGLITERALCLASS)
  }
}
