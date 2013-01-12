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
    if (expectedTokenClass != currentToken.tokenClass) {
      expected(expectedTokenClass);
    }
    readToken;
  }

  /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenClass */
  private def expected(tokenClass: TokenClass, more: TokenClass*): Nothing = {
    fatalError("expected: " + (tokenClass :: more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
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
   * 
   * object <id> { def main() : Unit = { <statement> } }
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
   * 
   * class <classId> [extends <extendz>] { <variables> <methods> }
   */
  def parseClasses: List[ClassDecl] = {
    var classes: List[ClassDecl] = List();

    while (currentToken.info != EOF) {
      val initial = currentToken;

      eat(CLASS);
      val classId = parseIdentifier;
      var extendz: Option[Identifier] = None;
      if (currentToken.info == EXTENDS) {
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

  /**
   * Parse variable declarations.
   * 
   * var <nameId> : <theType> ;
   */
  def parseVariablesDecl: List[VarDecl] = {
    var variables: List[VarDecl] = List();

    while (currentToken.info == VAR) {
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

  /**
   * Parse a type.
   */
  def parseType: TypeTree = {
    val initial = currentToken;
    
    // Multiple types, used in inline function types (FuncType)
    // (type, type)
    // Note: It has to be followed by an arrow and a return type (=> type)
    if(currentToken.info == OPAREN) {
        readToken
        var argTypeList: List[TypeTree] = List()
        argTypeList ::= parseType
        while (currentToken.info == COMMA) {
          readToken
          argTypeList ::= parseType
        }
        eat(CPAREN)
        
        eat(ARROW)
	    val returnType: TypeTree = parseType
	    
	    return new FuncType(argTypeList, Nil, returnType)
        
    } else {
        // Only one type
	    val firstType : TypeTree = currentToken.tokenClass match {
	      case INT =>
	        // Int[]
	        readToken;
	        if (currentToken.info == OBRACKET) {
	          eat(OBRACKET);
	          eat(CBRACKET);
	
	          new IntArrayType().setPos(initial);
	
	          // Int
	        } else {
	          new IntType().setPos(initial);
	        }
	
	      case STRING =>
	        // String
	        eat(STRING);
	        new StringType().setPos(currentToken);
	
	      case BOOL =>
	        eat(BOOL);
	        new BoolType().setPos(currentToken);
	        
	      case UNIT =>
	        eat(UNIT);
	        new UnitType().setPos(currentToken);
	
	      case IDCLASS =>
	        parseIdentifier;
	
	      case _ =>
	        expected(INT, STRING, BOOL, UNIT, IDCLASS);
	    }
	    
	    // Is it a FuncType?
	    // => returnType
	    if(currentToken.info == ARROW) {
	      eat(ARROW)
          val returnType: TypeTree = parseType
      
          return new FuncType(List(firstType), Nil, returnType)
	    } else {
	      return firstType;
	    }
    }
  }

  /**
   * Parse method declarations.
   */
  def parseMethodsDecl: List[MethodDecl] = {
    var methods: List[MethodDecl] = List();

    // Looping while we find def keywords
    while (currentToken.info == DEF) {
      val initial = currentToken;

      // Method name
      eat(DEF);
      val methodId = parseIdentifier;
      eat(OPAREN);

      // Arguments 
      var arguments: List[VarDecl] = List();
      if (currentToken.info != CPAREN) {
        while (currentToken.info != CPAREN) {
          if (arguments.length > 0)
            eat(COMMA);

          val argId = parseIdentifier;
          eat(COLON);

          val firstVarType = parseType;

          val arg = new VarDecl(argId, firstVarType).setPos(argId);
          arguments = arg :: arguments;
        }
      }
      arguments = arguments.reverse;
      eat(CPAREN);

      // Return Type
      eat(COLON);
      val returnType = parseType;
      eat(ASSIGN);
      eat(OBLOCK);

      // Variables

      val variables = parseVariablesDecl;

      // Statements
      var statements: List[StatTree] = List();
      while (currentToken.info != RETURN) {
        val stat = parseStatement;
        statements = stat :: statements;
      }
      statements = statements.reverse

      eat(RETURN);
      val returnExpr = parseExpression;
      eat(SEMICOLON);
      eat(CBLOCK);

      val method = new MethodDecl(methodId, arguments, returnType, variables, statements, returnExpr).setPos(initial);

      methods = method :: methods;
    }

    return methods.reverse;
  }

  /**
   * Parse a statement.
   */
  def parseStatement: StatTree = {

    currentToken.tokenClass match {
      case OBLOCK =>
        var list: List[StatTree] = List();
        readToken
        while (currentToken.info != CBLOCK || currentToken.info == EOF) {
          list = parseStatement :: list
        }
        readToken
        return new Block(list.reverse)

      // println ( expression )
      case PRINTLN =>
        readToken
        eat(OPAREN)
        val expr: ExprTree = parseExpression
        eat(CPAREN)
        eat(SEMICOLON)
        return new PrintLn(expr)

      // ident = expr
      case IDCLASS =>
        val ident: Identifier = parseIdentifier

        if (currentToken.info == OBRACKET) {
          eat(OBRACKET);
          val idx: ExprTree = parseExpression
          eat(CBRACKET)
          eat(ASSIGN)
          val expr: ExprTree = parseExpression
          eat(SEMICOLON)
          return new IndexAssignment(ident, idx, expr)
        } else {
          eat(ASSIGN)
          val expr: ExprTree = parseExpression
          eat(SEMICOLON)
          return new Assignment(ident, expr)
        }

      // if ( expr ) statmt (else statmt)?
      case IF =>
        eat(IF);
        eat(OPAREN)
        val expr: ExprTree = parseExpression
        eat(CPAREN)
        val stat: StatTree = parseStatement
        var elseStat: Option[StatTree] =
          if (currentToken.info == ELSE) {
            readToken
            new Some[StatTree](parseStatement)
          } else None
        return new If(expr, stat, elseStat)

      // while ( expr ) statmt
      case WHILE =>
        eat(WHILE);
        eat(OPAREN)
        val expr: ExprTree = parseExpression
        eat(CPAREN)
        val stat: StatTree = parseStatement
        return new While(expr, stat)

      case _ =>
        expected(OBLOCK, PRINTLN, IDCLASS, IF, WHILE)
    }
  }

  /**
   * Parse an expression.
   */
  def parseExpression: ExprTree = {
    return parseOrExpr;
  }

  /**
   * Parse an "or" expression.
   *
   * leftExpr || rightExpr
   */
  def parseOrExpr: ExprTree = {
    val leftExpr = parseAndExpr;
    //readToken;

    // leftExpr || rightExpr
    if (currentToken.info == OR) {
      eat(OR);
      val rightExpr = parseOrExpr;
      return new Or(leftExpr, rightExpr).setPos(leftExpr);

    } else {
      return leftExpr;
    }
  }

  /**
   * Parse an "and" expression.
   *
   * leftExpr && rightExpr
   */
  def parseAndExpr: ExprTree = {
    val leftExpr = parseEqualsLesserThanExpr;
    //readToken;

    // leftExpr && rightExpr
    if (currentToken.info == AND) {
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
  def parseEqualsLesserThanExpr: ExprTree = {
    var expr = parsePlusMinusExpr;

    while (currentToken.info == LESS || currentToken.info == EQUALS) {
      // leftExpr < rightExpr
      if (currentToken.info == LESS) {
        eat(LESS);
        expr = new LesserThan(expr, parsePlusMinusExpr).setPos(expr);

        // leftExpr == rightExpr
      } else if (currentToken.info == EQUALS) {
        eat(EQUALS);
        expr = new Equals(expr, parsePlusMinusExpr).setPos(expr);
      }
    }

    return expr;
  }

  /**
   * Parse an term expression.
   *
   * leftExpr + rightExpr
   * leftExpr - rightExpr
   */
  def parsePlusMinusExpr: ExprTree = {
    var expr = parseMultiplyDivideExpr;

    while (currentToken.info == PLUS || currentToken.info == MINUS) {
      // leftExpr + rightExpr
      if (currentToken.info == PLUS) {
        eat(PLUS);
        expr = new Plus(expr, parseMultiplyDivideExpr).setPos(expr);

        // leftExpr - rightExpr
      } else if (currentToken.info == MINUS) {
        eat(MINUS);
        expr = new Minus(expr, parseMultiplyDivideExpr).setPos(expr);
      }
    }
    return expr;
  }

  /**
   * Parse a factor expression.
   *
   * leftExpr * rightExpr
   * leftExpr / rightExpr
   */
  def parseMultiplyDivideExpr: ExprTree = {
    var expr = parseNotExpr;

    while (currentToken.info == MUL || currentToken.info == DIV) {
      // leftExpr * rightExpr
      if (currentToken.info == MUL) {
        eat(MUL);
        expr = new Multiply(expr, parseNotExpr).setPos(expr);

        // leftExpr / rightExpr
      } else if (currentToken.info == DIV) {
        eat(DIV);
        expr = new Divide(expr, parseNotExpr).setPos(expr);
      }
    }

    return expr;
  }

  /**
   * Parse a unary ! expression.
   *
   * !rightExpr
   */
  def parseNotExpr: ExprTree = {
    val initial = currentToken;

    // !rightExpr
    if (currentToken.info == BANG) {
      eat(BANG);
      val rightExpr = parseNotExpr;
      return new Not(rightExpr).setPos(initial);

    } else {
      return parseIndexExpr;
    }
  }

  /**
   * Parse an indexed (access to an array) expression.
   *
   * leftExpr[expr]
   */
  def parseIndexExpr: ExprTree = {
    val leftExpr = parseDotExpr;

    // leftExpr[expr]
    if (currentToken.info == OBRACKET) {
      eat(OBRACKET);
      var index = parseExpression;
      eat(CBRACKET);

      return new Index(leftExpr, index).setPos(leftExpr);

    } else {
      return leftExpr;
    }
  }

  /**
   * Parse a dot expression.
   *
   * leftExpr.length
   * leftExpr.methodId(parameters...)
   */
  def parseDotExpr: ExprTree = {
    var expr = parseFuncCallExpr;

    // leftExpr.
    if (currentToken.info == DOT) {
      while (currentToken.info == DOT) {
        eat(DOT);

        // leftExpr.length
        if (currentToken.info == LENGTH) {
          eat(LENGTH);
          expr = new Length(expr).setPos(expr);

          // leftExpr.methodId(paramList)
        } else {
          val methodId = parseIdentifier;
          val paramList = parseParametersList;
          expr = new MethodCall(expr, methodId, paramList).setPos(expr)
        }
      }
    }

    return expr;
  }
  
  /**
   * Parse a call expression on a function expression.
   * 
   * leftExpr(arguments)
   */
  def parseFuncCallExpr: ExprTree = {
    var expr = parseSimpleExpr;
    
    // leftExpr(
    if (currentToken.info == OPAREN) {
      while (currentToken.info == OPAREN) {
          // leftExpr(parameterList...)
    	  val paramList = parseParametersList
          expr = new FuncCall(expr, paramList);
      }
    }
    
    return expr;
  }

  /**
   * Parse a list of parameters.
   *
   * (expr, expr, expr)
   */
  def parseParametersList: List[ExprTree] = {
    var list: List[ExprTree] = List();

    eat(OPAREN);
    if (currentToken.info != CPAREN) {
      val expr = parseExpression;
      list = expr :: list;

      while (currentToken.info == COMMA) {
        eat(COMMA);

        val expr = parseExpression;
        list = expr :: list;
      }
    }

    eat(CPAREN);

    return list.reverse;
  }

  /**
   * Parse a function expression (inline declaration).
   * 
   * => { <variables> <statements> return <returnExpr>; }
   */
  def parseFuncExpr(args: List[VarDecl]): FuncExpr = {
    eat(ARROW);
    eat(OBLOCK);

    // Variables
    val variables = parseVariablesDecl;

    // Statements
    var statements: List[StatTree] = List();
    while (currentToken.info != RETURN && currentToken.info != CBLOCK) {
      val stat = parseStatement;
      statements = stat :: statements;
    }
    statements = statements.reverse

    if(currentToken.info == RETURN) {
	    eat(RETURN);
	    val returnExpr = parseExpression;
	    eat(SEMICOLON);
	    eat(CBLOCK);
	    
	    new FuncExpr(args, variables, statements, Some(returnExpr));
    } else {
    	eat(CBLOCK);
    	
    	new FuncExpr(args, variables, statements, None); 
    }
  }

  /**
   * Parse a "simple" expression, such as parenthesis, identifier, integer literal,
   * string literal, boolean literal, etc.
   */
  def parseSimpleExpr: ExprTree = {
    val initial = currentToken;

    currentToken.tokenClass match {
      
      // ( expr ) 
      // () => { ... } 
      // ( arg1 : type1... ) => { ... } 
      case OPAREN =>
        eat(OPAREN);
        
        // () => { ... }
        if(currentToken.info == CPAREN) {
          eat(CPAREN);
          return parseFuncExpr(List())
        } else {
          // ( expr
          val expr = parseExpression;

          // ( expr : ... ) => { ... }
          if (currentToken.info == COLON && expr.isInstanceOf[Identifier]) {
            val ident = expr.asInstanceOf[Identifier]
            eat(COLON)
            val argType = parseType
            var list: List[VarDecl] = List(new VarDecl(ident, argType));
            while (currentToken.info == COMMA) {
              eat(COMMA)
              val ident = parseIdentifier
              eat(COLON)
              val argType = parseType
              list = new VarDecl(ident, argType) :: list
            }
            eat(CPAREN);
            return parseFuncExpr(list)
          
          // ( expr )
          } else {
            eat(CPAREN);
            return expr
          }
        }

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

        // new Int[expr]
        if (currentToken.info == INT) {
          eat(INT);
          eat(OBRACKET);
          val expr = parseExpression;
          eat(CBRACKET);
          return new NewArray(expr).setPos(initial);

          // new id()
        } else {
          val id = parseIdentifier;
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
