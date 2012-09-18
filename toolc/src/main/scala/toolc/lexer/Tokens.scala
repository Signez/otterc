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

  // etc.

  case object PRINTLN extends TokenInfo with TokenClass

  // Identifiers
  case class ID(value: String) extends TokenInfo {
    override def toString: String = "ID"+value
    def tokenClass: TokenClass = IDCLASS
  }

  // etc.

  // The token class for identifiers
  case object IDCLASS extends TokenClass {
    override def toString: String = "identifier"
  }

  // etc.

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
