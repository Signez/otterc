package toolc
package lexer

import toolc.Positional

object Tokens {
  /** The info attached to a token (beside the position). For most
  tokens this is just the class information (eg. "it's a colon").
  For literals and identifiers, this also contains the proper
  information (eg. "this number literal represents 42"). */
  sealed trait TokenInfo {
    def tokenClass: TokenClass
  }

  /** All tokens have a corresponding token class: for instance,
  all identifiers have the IDCLASS. For "informationless"
  tokens (like COLON), the token class is the token info itself.
  This is why the trait TokenClass is mixed-in with the class in
  that case. This will be useful for the parser, the next lab.
  */
  sealed trait TokenClass {
    self =>

    def tokenClass: self.type = self
  }

  case object BAD extends TokenInfo with TokenClass             // represents incorrect tokens.
  case object EOF extends TokenInfo with TokenClass             // represents end of file
  case object COLON extends TokenInfo with TokenClass           // :
  case object COMMA extends TokenInfo with TokenClass			// ,
  case object DOT extends TokenInfo with TokenClass				// .
  case object BANG extends TokenInfo with TokenClass			// !
  case object AND extends TokenInfo with TokenClass 			// &&
  case object OR extends TokenInfo with TokenClass				// ||
  case object EQUALS extends TokenInfo with TokenClass			// ==
  case object LESS extends TokenInfo with TokenClass		    // <
  case object PLUS extends TokenInfo with TokenClass			// +
  case object MINUS extends TokenInfo with TokenClass			// -
  case object MUL extends TokenInfo with TokenClass				// *
  case object DIV extends TokenInfo with TokenClass				// /
  case object LENGTH extends TokenInfo with TokenClass			// length
  case object OBRACKET extends TokenInfo with TokenClass 		// [
  case object CBRACKET extends TokenInfo with TokenClass 		// ]
  case object OPAREN extends TokenInfo with TokenClass 		    // (
  case object CPAREN extends TokenInfo with TokenClass 		    // )
  case object OBLOCK extends TokenInfo with TokenClass 		    // {
  case object CBLOCK extends TokenInfo with TokenClass 		    // }
  case object TRUE extends TokenInfo with TokenClass 		    // true
  case object FALSE extends TokenInfo with TokenClass 		    // false
  case object THIS extends TokenInfo with TokenClass 		    // this
  case object NEW extends TokenInfo with TokenClass 		    // new
  case object ASSIGN extends TokenInfo with TokenClass 		    // =
  case object PRINTLN extends TokenInfo with TokenClass			// println
  case object WHILE extends TokenInfo with TokenClass			// while
  case object IF extends TokenInfo with TokenClass				// if
  case object ELSE extends TokenInfo with TokenClass			// else
  case object SEMICOLON extends TokenInfo with TokenClass		// ;
  case object DEF extends TokenInfo with TokenClass				// def
  case object VAR extends TokenInfo with TokenClass				// var
  case object CLASS extends TokenInfo with TokenClass			// class
  case object EXTENDS extends TokenInfo with TokenClass			// extends
  case object RETURN extends TokenInfo with TokenClass			// return
  case object OBJECT extends TokenInfo with TokenClass			// object
  case object MAIN extends TokenInfo with TokenClass			// main
  case object INT extends TokenInfo with TokenClass				// Int
  case object STRING extends TokenInfo with TokenClass			// String
  case object BOOL extends TokenInfo with TokenClass			// Bool
  case object UNIT extends TokenInfo with TokenClass			// Unit
  case object ARROW extends TokenInfo with TokenClass			// =>

  // Identifiers
  case class ID(value: String) extends TokenInfo {
    override def toString: String = "ID('" + value + "')"
    def tokenClass: TokenClass = IDCLASS
  }

  // Integer literals
  case class INTEGERLITERAL(value: Int) extends TokenInfo {
    override def toString: String = "INTEGER_LITERAL(" + value + ")"
    def tokenClass: TokenClass = INTEGERLITERALCLASS
  }

  // String literals
  case class STRINGLITERAL(value: String) extends TokenInfo {
    override def toString: String = "STRING_LITERAL(\"" + value + "\")"
    def tokenClass: TokenClass = STRINGLITERALCLASS
  }
  
  // The token class for identifiers
  case object IDCLASS extends TokenClass {
    override def toString: String = "identifier"
  }
  
  // Token class for integer
  case object INTEGERLITERALCLASS extends TokenClass {
    override def toString: String = "integer litteral"
  }
  
  // Token 
  case object STRINGLITERALCLASS extends TokenClass {
    override def toString: String = "string litteral"
  }

  /** A Token is a positional wrapper around an info.
  You don't have to change anything in that class. */
  class Token(val info: TokenInfo) extends Positional {
    override def toString: String = info.toString
    def tokenClass: TokenClass = info.tokenClass
  }

  /** To create tokens and match on them. Nothing to change here. */
  object Token {
    def apply(info: TokenInfo): Token = new Token(info)
    def unapply(token: Token): Option[TokenInfo] = Some(token.info)
  }
}
