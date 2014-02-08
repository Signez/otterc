package otterc
package parser

import lexer.Lexer
import otterc.lexer.Tokens.IDCLASS

/**
 * LL parser for the Otter grammar.
 *
 * Uses tokens from Tokens companion object.
 **/
trait Parser extends Lexer {
  self: Compiler =>

  import Trees._
  import lexer.Tokens._

  def parseSource: Tree = {
    readToken() // initializes the parser by calling the method in the Lexer.
    val tree: Tree = parseProgram
    terminateIfErrors()
    tree
  }

  /**
   * Current token read from the lexer.
   */
  private var currentToken: Token = Token(BAD).setPosition(0)

  /**
   * Previous token read from the lexer (before currentToken).
   */
  private var lastToken: Token = currentToken

  /**
   * Read a new token from the Lexer, saving it into currentToken (and saving
   * previous one on lastToken).
   *
   * Newline are silently ignored (they should never be the currentToken, but
   * may be the lastToken).
   */
  private def readToken(): Unit = {
    do {
      lastToken = currentToken

      // Update currentToken using nextToken in the Lexer
      currentToken = nextToken
    } while (currentToken.tokenClass == NEWLINE)

    currentToken
  }

  /**
   * Read a token if it is expected, or terminates with an error.
   */
  private def eat(expectedTokenClass: TokenClass): Unit = {
    expectedTokenClass match {
      case SEMICOLON =>
        if (currentToken.tokenClass != SEMICOLON && lastToken.tokenClass != NEWLINE) {
          expected(SEMICOLON, NEWLINE)
        }
        if (currentToken.tokenClass == SEMICOLON) {
          readToken()
        }

      case _ =>
        if (expectedTokenClass != currentToken.tokenClass) {
          expected(expectedTokenClass)
        }
        readToken()
    }
  }

  /**
   * Complains that what was found was not expected.
   *
   * The method accepts arbitrarily many arguments of type TokenClass.
   */
  private def expected(tokenClass: TokenClass, more: TokenClass*): Nothing = {
    fatalError("expected: " + (tokenClass :: more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
  }

  /**
   * Parse the whole program.
   */
  private def parseProgram: Program = {
    val main = parseMainObject
    val classes = parseClasses

    new Program(main, classes).setPosition(main)
  }

  /**
   * Parse main object.
   *
   * object <id> { def main() : Unit = { <statement> } }
   */
  def parseMainObject: MainObject = {
    val initial = currentToken

    // object id
    eat(OBJECT)
    val id = parseIdentifier
    // { def main() : Unit = { stat
    eat(OBLOCK)
    eat(DEF)
    eat(MAIN)
    eat(OPAREN)
    eat(CPAREN)
    eat(COLON)
    eat(UNIT)
    eat(ASSIGN)
    eat(OBLOCK)
    val stat = parseStatement
    // }}
    eat(CBLOCK)
    eat(CBLOCK)

    new MainObject(id, stat).setPosition(initial)
  }

  /**
   * Parse classes.
   *
   * class <classId> [extends <extendz>] { <variables> <methods> }
   */
  def parseClasses: List[ClassDecl] = {
    var classes: List[ClassDecl] = List()

    while (currentToken.metadata != EOF) {
      val initial = currentToken

      eat(CLASS)
      val classId = parseIdentifier
      var extendz: Option[Identifier] = None
      if (currentToken.metadata == EXTENDS) {
        eat(EXTENDS)
        extendz = Some(parseIdentifier)
      }
      eat(OBLOCK)
      val variables = parseVariablesDecl
      val methods = parseMethodsDecl
      eat(CBLOCK)

      val classDecl = new ClassDecl(classId, extendz, variables, methods).setPosition(initial)
      classes = classDecl :: classes
    }

    classes.reverse
  }

  /**
   * Parse variable declarations.
   *
   * var <nameId> : <theType> ;
   */
  def parseVariablesDecl: List[VarDecl] = {
    var variables: List[VarDecl] = List()

    while (currentToken.metadata == VAR) {
      val initial = currentToken

      eat(VAR)
      val nameId = parseIdentifier
      eat(COLON)
      val theType = parseType
      eat(SEMICOLON)

      val variable = new VarDecl(nameId, theType).setPosition(initial)

      variables = variable :: variables
    }

    variables.reverse
  }

  /**
   * Parse a type.
   */
  def parseType: TypeTree = {
    val initial = currentToken

    // Only one type
    val simpleType: TypeTree = currentToken.tokenClass match {
      case INT =>
        // Int[]
        readToken()
        if (currentToken.metadata == OBRACKET) {
          eat(OBRACKET)
          eat(CBRACKET)

          new IntArrayType().setPosition(initial)

          // Int
        } else {
          new IntType().setPosition(initial)
        }

      case STRING =>
        // String
        eat(STRING)
        new StringType().setPosition(currentToken)

      case BOOL =>
        eat(BOOL)
        new BoolType().setPosition(currentToken)

      case UNIT =>
        eat(UNIT)
        new UnitType().setPosition(currentToken)

      case IDCLASS =>
        parseIdentifier

      case _ =>
        expected(INT, STRING, BOOL, UNIT, IDCLASS)
    }

    simpleType
  }

  /**
   * Parse method declarations.
   */
  def parseMethodsDecl: List[MethodDecl] = {
    var methods: List[MethodDecl] = List()

    // Looping while we find def keywords
    while (currentToken.metadata == DEF) {
      val initial = currentToken

      // Method name
      eat(DEF)
      val methodId = parseIdentifier
      eat(OPAREN)

      // Arguments 
      var arguments: List[VarDecl] = List()
      if (currentToken.metadata != CPAREN) {
        while (currentToken.metadata != CPAREN) {
          if (arguments.length > 0)
            eat(COMMA)

          val argId = parseIdentifier
          eat(COLON)

          val firstVarType = parseType

          val arg = new VarDecl(argId, firstVarType).setPosition(argId)
          arguments = arg :: arguments
        }
      }
      arguments = arguments.reverse
      eat(CPAREN)

      // Return Type
      eat(COLON)
      val returnType = parseType
      eat(ASSIGN)
      eat(OBLOCK)

      // Variables

      val variables = parseVariablesDecl

      // Statements
      var statements: List[StatementTree] = List()
      while (currentToken.metadata != RETURN) {
        val stat = parseStatement
        statements = stat :: statements
      }
      statements = statements.reverse

      var returnExpr: Option[ExpressionTree] = None

      if (currentToken.metadata == RETURN) {
        eat(RETURN)
        returnExpr = Some(parseExpression)
        eat(SEMICOLON)
        eat(CBLOCK)
      }

      val method = new MethodDecl(methodId, arguments, returnType, variables, statements, returnExpr).setPosition(initial)

      methods = method :: methods
    }

    methods.reverse
  }

  /**
   * Parse a statement.
   */
  def parseStatement: StatementTree = {

    currentToken.tokenClass match {
      case OBLOCK =>
        var list: List[StatementTree] = List()
        readToken()
        while (currentToken.metadata != CBLOCK || currentToken.metadata == EOF) {
          list = parseStatement :: list
        }
        readToken()
        new Block(list.reverse)

      // println ( expression )
      case PRINTLN =>
        readToken()
        eat(OPAREN)
        val expr: ExpressionTree = parseExpression
        eat(CPAREN)
        eat(SEMICOLON)
        new PrintLn(expr)

      // if ( expr ) statmt (else statmt)?
      case IF =>
        eat(IF)
        eat(OPAREN)
        val expr: ExpressionTree = parseExpression
        eat(CPAREN)
        val stat: StatementTree = parseStatement
        val elseStat: Option[StatementTree] =
          if (currentToken.metadata == ELSE) {
            readToken()
            new Some[StatementTree](parseStatement)
          } else None
        new If(expr, stat, elseStat)

      // while ( expr ) statmt
      case WHILE =>
        eat(WHILE)
        eat(OPAREN)
        val expr: ExpressionTree = parseExpression
        eat(CPAREN)
        val stat: StatementTree = parseStatement
        new While(expr, stat)

      case OPAREN | IDCLASS | INTEGERLITERALCLASS | STRINGLITERALCLASS | TRUE | FALSE | THIS | NEW =>
        val expr: ExpressionTree = parseExpression
        eat(SEMICOLON)
        new UnitExpression(expr)

      case _ =>
        expected(OBLOCK, PRINTLN, IDCLASS, IF, WHILE, OPAREN, INTEGERLITERALCLASS, STRINGLITERALCLASS, TRUE, FALSE, THIS, NEW)
    }
  }

  /**
   * Parse an expression.
   */
  def parseExpression: ExpressionTree = {
    parseOrExpr
  }

  /**
   * Parse an "or" expression.
   *
   * leftExpr || rightExpr
   */
  def parseOrExpr: ExpressionTree = {
    val leftExpr = parseAndExpr
    //readToken;

    // leftExpr || rightExpr
    if (currentToken.metadata == OR) {
      eat(OR)
      val rightExpr = parseOrExpr
      new Or(leftExpr, rightExpr).setPosition(leftExpr)

    } else {
      leftExpr
    }
  }

  /**
   * Parse an "and" expression.
   *
   * leftExpr && rightExpr
   */
  def parseAndExpr: ExpressionTree = {
    val leftExpr = parseEqualsLesserThanExpr
    //readToken;

    // leftExpr && rightExpr
    if (currentToken.metadata == AND) {
      eat(AND)
      val rightExpr = parseAndExpr
      new And(leftExpr, rightExpr).setPosition(leftExpr)

    } else {
      leftExpr
    }
  }

  /**
   * Parse a == or a < expression.
   *
   * leftExpr < rightExpr
   * leftExpr == rightExpr
   */
  def parseEqualsLesserThanExpr: ExpressionTree = {
    var expr = parsePlusMinusExpr

    while (currentToken.metadata == LESS || currentToken.metadata == EQUALS) {
      // leftExpr < rightExpr
      if (currentToken.metadata == LESS) {
        eat(LESS)
        expr = new LesserThan(expr, parsePlusMinusExpr).setPosition(expr)

        // leftExpr == rightExpr
      } else if (currentToken.metadata == EQUALS) {
        eat(EQUALS)
        expr = new Equals(expr, parsePlusMinusExpr).setPosition(expr)
      }
    }

    expr
  }

  /**
   * Parse an term expression.
   *
   * leftExpr + rightExpr
   * leftExpr - rightExpr
   */
  def parsePlusMinusExpr: ExpressionTree = {
    var expr = parseMultiplyDivideExpr

    while (currentToken.metadata == PLUS || currentToken.metadata == MINUS) {
      // leftExpr + rightExpr
      if (currentToken.metadata == PLUS) {
        eat(PLUS)
        expr = new Plus(expr, parseMultiplyDivideExpr).setPosition(expr)

        // leftExpr - rightExpr
      } else if (currentToken.metadata == MINUS) {
        eat(MINUS)
        expr = new Minus(expr, parseMultiplyDivideExpr).setPosition(expr)
      }
    }
    expr
  }

  /**
   * Parse a factor expression.
   *
   * leftExpr * rightExpr
   * leftExpr / rightExpr
   */
  def parseMultiplyDivideExpr: ExpressionTree = {
    var expr = parseNotExpr

    while (currentToken.metadata == MUL || currentToken.metadata == DIV) {
      // leftExpr * rightExpr
      if (currentToken.metadata == MUL) {
        eat(MUL)
        expr = new Multiply(expr, parseNotExpr).setPosition(expr)

        // leftExpr / rightExpr
      } else if (currentToken.metadata == DIV) {
        eat(DIV)
        expr = new Divide(expr, parseNotExpr).setPosition(expr)
      }
    }

    expr
  }

  /**
   * Parse a unary ! expression.
   *
   * !rightExpr
   */
  def parseNotExpr: ExpressionTree = {
    val initial = currentToken

    // !rightExpr
    if (currentToken.metadata == BANG) {
      eat(BANG)
      val rightExpr = parseNotExpr
      new Not(rightExpr).setPosition(initial)

    } else {
      parseIndexExpr
    }
  }

  /**
   * Parse an indexed (access to an array) expression.
   *
   * leftExpr[expr]
   */
  def parseIndexExpr: ExpressionTree = {
    val leftExpr = parseDotExpr

    // leftExpr[expr]
    if (currentToken.metadata == OBRACKET) {
      eat(OBRACKET)
      val index = parseExpression
      eat(CBRACKET)

      new Index(leftExpr, index).setPosition(leftExpr)

    } else {
      leftExpr
    }
  }

  /**
   * Parse a dot expression.
   *
   * leftExpr.length
   * leftExpr.methodId(parameters...)
   */
  def parseDotExpr: ExpressionTree = {
    var expr = parseAssignment

    // leftExpr.
    if (currentToken.metadata == DOT) {
      while (currentToken.metadata == DOT) {
        eat(DOT)

        // leftExpr.length
        if (currentToken.metadata == LENGTH) {
          eat(LENGTH)
          expr = new Length(expr).setPosition(expr)

          // leftExpr.methodId(paramList)
        } else {
          val methodId = parseIdentifier
          val paramList = parseParametersList
          expr = new MethodCall(expr, methodId, paramList).setPosition(expr)
        }
      }
    }

    expr
  }

  /**
   * Parse an assignment.
   *
   * target = expression
   *
   * @return
   */
  def parseAssignment: ExpressionTree = {
    val target = parseSimpleExpr

    target match {
      case id@Identifier(_) =>
        if (currentToken.metadata == ASSIGN) {
          eat(ASSIGN)

          val rightExpr = parseExpression
          new Assignment(id, rightExpr)

        } else if (currentToken.metadata == OBRACKET) {
          eat(OBRACKET)

          val idx: ExpressionTree = parseExpression
          eat(CBRACKET)

          if (currentToken.metadata == ASSIGN) {
            eat(ASSIGN)

            val expr: ExpressionTree = parseExpression

            new IndexAssignment(id, idx, expr)
          } else {
            new Index(id, idx).setPosition(target)
          }
        } else {
          target
        }

      case _ => target
    }
  }

  /**
   * Parse a list of parameters.
   *
   * (expr, expr, expr)
   */
  def parseParametersList: List[ExpressionTree] = {
    var list: List[ExpressionTree] = List()

    eat(OPAREN)
    if (currentToken.metadata != CPAREN) {
      val expr = parseExpression
      list = expr :: list

      while (currentToken.metadata == COMMA) {
        eat(COMMA)

        val expr = parseExpression
        list = expr :: list
      }
    }

    eat(CPAREN)

    list.reverse
  }

  /**
   * Parse a "simple" expression, such as parenthesis, identifier, integer literal,
   * string literal, boolean literal, etc.
   */
  def parseSimpleExpr: ExpressionTree = {
    val initial = currentToken

    currentToken.tokenClass match {

      // ( expr ) 
      case OPAREN =>
        eat(OPAREN)

        // ( expr
        val expr = parseExpression

        eat(CPAREN)
        expr

      // identifier
      case IDCLASS =>
        parseIdentifier

      // integer literal
      case INTEGERLITERALCLASS =>
        parseIntegerLiteral

      // string literal
      case STRINGLITERALCLASS =>
        parseStringLiteral

      // boolean literals
      case TRUE =>
        val expr = BooleanLiteral(value = true).setPosition(currentToken)
        readToken()
        expr
      case FALSE =>
        val expr = BooleanLiteral(value = false).setPosition(currentToken)
        readToken()
        expr

      // this
      case THIS =>
        eat(THIS)
        ThisObject().setPosition(currentToken)

      case NEW =>
        eat(NEW)

        // new Int[expr]
        if (currentToken.metadata == INT) {
          eat(INT)
          eat(OBRACKET)
          val expr = parseExpression
          eat(CBRACKET)
          new NewArray(expr).setPosition(initial)

          // new id()
        } else {
          val id = parseIdentifier
          eat(OPAREN)
          eat(CPAREN)
          new NewObject(id).setPosition(initial)
        }

      case _ =>
        expected(OPAREN, IDCLASS, INTEGERLITERALCLASS, STRINGLITERALCLASS, TRUE, FALSE, THIS, NEW)
    }
  }

  /**
   * Parse an identifier.
   */
  private def parseIdentifier: Identifier = currentToken.metadata match {
    case ID(value) =>
      val ret = Identifier(value).setPosition(currentToken)
      readToken()
      ret
    case _ => expected(IDCLASS)
  }

  /**
   * Parse an integer literal.
   */
  private def parseIntegerLiteral: IntegerLiteral = currentToken.metadata match {
    case INTEGERLITERAL(value) =>
      val ret = IntegerLiteral(value).setPosition(currentToken)
      readToken()
      ret
    case _ => expected(INTEGERLITERALCLASS)
  }

  /**
   * Parse a string literal.
   */
  private def parseStringLiteral: StringLiteral = currentToken.metadata match {
    case STRINGLITERAL(value) =>
      val ret = StringLiteral(value).setPosition(currentToken)
      readToken()
      ret
    case _ => expected(STRINGLITERALCLASS)
  }
}
