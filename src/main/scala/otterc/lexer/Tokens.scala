package otterc
package lexer

import otterc.Positional

object Tokens {

  /**
   * Token.
   *
   * A token is a piece of read code. Created by the lexer, it
   * will be used as "lego pieces" by the parser to create a tree
   * containing statements and expressions.
   *
   * Its position is persisted via Positional, and its metadata
   * (what it is and what it represents) is saved in its metadata
   * class parameter.
   *
   * @see otterc.Positional
   * @param metadata Token metadata.
   */
  class Token(val metadata: TokenMetadata) extends Positional {
    override def toString: String = metadata.toString

    def tokenClass: TokenClass = metadata.tokenClass
  }

  /**
   * Token metadata abstract trait.
   *
   * These metadata are linked to a token; most of the times, it's
   * simply an abstract trait that will be extended by a case class
   * that represents the token kind (BANG, AND, etc.).
   *
   * For a few tokens, some additional data have to be persisted
   * (identifiers and literals) that will be used later on the
   * compiling process, such as identifier name.
   */
  sealed trait TokenMetadata {
    def tokenClass: TokenClass
  }

  /**
   * Simple token class.
   *
   * For simple tokens, the token type itself is all the information
   * needed for parser ("&&" string is an AND token and that's it).
   *
   * This is syntaxic sugar for all following token declarations.
   */
  sealed trait TokenClass {
    self =>

    def tokenClass: self.type = self
  }

  /**
   * Incorrect token.
   */
  case object BAD extends TokenMetadata with TokenClass

  /**
   * End of file.
   */
  case object EOF extends TokenMetadata with TokenClass

  /** : */
  case object COLON extends TokenMetadata with TokenClass

  /** , */
  case object COMMA extends TokenMetadata with TokenClass

  /** . */
  case object DOT extends TokenMetadata with TokenClass

  /** ! */
  case object BANG extends TokenMetadata with TokenClass

  /** && */
  case object AND extends TokenMetadata with TokenClass

  /** || */
  case object OR extends TokenMetadata with TokenClass

  /** '==' */
  case object EQUALS extends TokenMetadata with TokenClass

  /** < */
  case object LESS extends TokenMetadata with TokenClass

  /** + */
  case object PLUS extends TokenMetadata with TokenClass

  /** - */
  case object MINUS extends TokenMetadata with TokenClass

  /** * */
  case object MUL extends TokenMetadata with TokenClass

  /** / */
  case object DIV extends TokenMetadata with TokenClass

  /** length keyword */
  case object LENGTH extends TokenMetadata with TokenClass

  /** [ */
  case object OBRACKET extends TokenMetadata with TokenClass

  /** ] */
  case object CBRACKET extends TokenMetadata with TokenClass

  /** ( */
  case object OPAREN extends TokenMetadata with TokenClass

  /** ) */
  case object CPAREN extends TokenMetadata with TokenClass

  /** { */
  case object OBLOCK extends TokenMetadata with TokenClass

  /** } */
  case object CBLOCK extends TokenMetadata with TokenClass

  /** true */
  case object TRUE extends TokenMetadata with TokenClass

  /** false */
  case object FALSE extends TokenMetadata with TokenClass

  /** this */
  case object THIS extends TokenMetadata with TokenClass

  /** new */
  case object NEW extends TokenMetadata with TokenClass

  /** '=' */
  case object ASSIGN extends TokenMetadata with TokenClass

  /** println */
  case object PRINTLN extends TokenMetadata with TokenClass

  /** while */
  case object WHILE extends TokenMetadata with TokenClass

  /** if */
  case object IF extends TokenMetadata with TokenClass

  /** else */
  case object ELSE extends TokenMetadata with TokenClass

  /** ; */
  case object SEMICOLON extends TokenMetadata with TokenClass

  /** def */
  case object DEF extends TokenMetadata with TokenClass

  /** var */
  case object VAR extends TokenMetadata with TokenClass

  /** class */
  case object CLASS extends TokenMetadata with TokenClass

  /** extends */
  case object EXTENDS extends TokenMetadata with TokenClass

  /** return */
  case object RETURN extends TokenMetadata with TokenClass

  /** object */
  case object OBJECT extends TokenMetadata with TokenClass

  /** main */
  case object MAIN extends TokenMetadata with TokenClass

  /** Int */
  case object INT extends TokenMetadata with TokenClass

  /** String */
  case object STRING extends TokenMetadata with TokenClass

  /** Bool */
  case object BOOL extends TokenMetadata with TokenClass

  /** Unit */
  case object UNIT extends TokenMetadata with TokenClass

  /** '=>' */
  case object ARROW extends TokenMetadata with TokenClass

  /** Newline (\n) */
  case object NEWLINE extends TokenMetadata with TokenClass

  /**
   * Identifier.
   *
   * @param value Text.
   */
  case class ID(value: String) extends TokenMetadata {
    override def toString: String = "ID('" + value + "')"

    def tokenClass: TokenClass = IDCLASS
  }

  /**
   * Integer literal.
   *
   * @param value Number.
   */
  case class INTEGERLITERAL(value: Int) extends TokenMetadata {
    override def toString: String = "INTEGER_LITERAL(" + value + ")"

    def tokenClass: TokenClass = INTEGERLITERALCLASS
  }

  /**
   * String literal.
   *
   * @param value String.
   */
  case class STRINGLITERAL(value: String) extends TokenMetadata {
    override def toString: String = "STRING_LITERAL(\"" + value + "\")"

    def tokenClass: TokenClass = STRINGLITERALCLASS
  }

  /**
   * Identifier token.
   */
  case object IDCLASS extends TokenClass {
    override def toString: String = "identifier"
  }

  /**
   * Integer literal token.
   */
  case object INTEGERLITERALCLASS extends TokenClass {
    override def toString: String = "integer literal"
  }

  /**
   * String literal token.
   */
  case object STRINGLITERALCLASS extends TokenClass {
    override def toString: String = "string literal"
  }

  /**
   * XXXX: Understand what this is?!?
   */
  object Token {
    def apply(info: TokenMetadata): Token = new Token(info)

    def unapply(token: Token): Option[TokenMetadata] = Some(token.metadata)
  }

}
