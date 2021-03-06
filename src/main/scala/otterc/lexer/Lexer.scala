package otterc
package lexer

import scala.io.Source
import scala.annotation.tailrec

trait Lexer {
  // This trait will be composed with a Compiler. 
  // Implies that the methods from the Reporter class will be available at run-time, 
  // as well as the reference to the Source object.
  self: Compiler =>

  import Tokens._

  /**
   * End of File: this boolean is true when we are not enable to go to
   * the next character (because the streams ends).
   */
  var isEOF = false

  /**
   * Contains all keywords, i.e. special identifiers that are reserved
   * and used as part of the language.
   *
   * Note that doesn't contain any "primitive" types as they are NOT 
   * keywords but special type identifiers handled in the Parser layer.
   */
  val keywordMap = Map[String, Token](
    "if" -> Token(IF),
    "else" -> Token(ELSE),
    "extends" -> Token(EXTENDS),
    "length" -> Token(LENGTH),
    "true" -> Token(TRUE),
    "false" -> Token(FALSE),
    "this" -> Token(THIS),
    "new" -> Token(NEW),
    "println" -> Token(PRINTLN),
    "while" -> Token(WHILE),
    "def" -> Token(DEF),
    "var" -> Token(VAR),
    "class" -> Token(CLASS),
    "extends" -> Token(EXTENDS),
    "return" -> Token(RETURN),
    "object" -> Token(OBJECT),
    "main" -> Token(MAIN),
    "Int" -> Token(INT),
    "String" -> Token(STRING),
    "Bool" -> Token(BOOL),
    "Unit" -> Token(UNIT)
  )

  /**
   * Read an identifier from the source, consuming the stream until neither 
   * a letter nor a digit shows up. 
   */
  def readIdentifier(): String = {
    var buffer = ""
    while (source.ch.isLetterOrDigit || source.ch == '_') {
      buffer += source.ch
      source.next()
    }
    buffer
  }

  /**
   * Read an integer from the source, consuming the stream until a non-digit 
   * character shows up.
   */
  def readInteger(): Int = {
    var buffer = ""
    while (source.ch.isDigit) {
      buffer += source.ch
      source.next()
    }
    buffer.toInt
  }

  /**
   * Read any character until `endChar` shows up in the stream, or 
   * a newline is encountered.
   */
  def readEverythingUntil(endChar: Char): String = {
    var buffer = ""
    while (source.ch != -1 && source.ch != endChar && source.ch != '\n') {
      buffer += source.ch
      source.next()
    }
    if (source.ch == '\n') {
      error("Unexpected newline encountered in string literal.")
    }
    buffer
  }

  /**
   * Consume all whitespaces characters from the stream, and stop
   * when a non-whitespace characters shows up.
   */
  def trimWhitespaces(): Unit = {
    while (source.ch.isWhitespace && source.ch != '\r' && source.ch != '\n'
      && source.hasNext)
      source.next()
  }

  /**
   * Read the stream to find a new token.
   */
  private def readNewToken(): Token = {
    var pos = 1
    var token = Token(BAD)

    if (source.pos == 0 && source.hasNext) source.next()

    // WHITESPACES ========================================
    // Trimming whitespaces
    trimWhitespaces()

    // EOF ================================================
    // If we are at end of file, let's return EOF
    if (isEOF) {
      return Token(EOF).setPosition(source.pos)
    } else if (!source.hasNext) {
      // The last character was not consumed.
      // Let's flag this as the last loop
      isEOF = true
    }

    // NEWLINES ===========================================
    // Eat CR for Windows users
    if (source.ch == '\r') {
      if (source.hasNext) {
        source.next()
      }

      if(source.ch != '\n') {
        fatalError(" - Unexpected CR non-followed by a LF", source.pos)
      }
    }

    // Create NEWLINE tokens on newlines
    if (source.ch == '\n') {
      if (source.hasNext) {
        source.next()
      }
      return Token(NEWLINE).setPosition(source.pos)
    }

    // IDENTIFIERS & KEYWORDS =============================
    // All identifiers and keywords starts with letters
    if (source.ch.isLetter) {
      pos = source.pos
      val sourceString = readIdentifier()
      // Get the corresponding keyword-token if it exists, else return a simple identifier
      return keywordMap.get(sourceString).getOrElse[Token](Token(ID(sourceString))).setPosition(pos)
    }

    // STRINGS LITERALS ====================================
    if (source.ch == '"') {
      // We found the first double-quote : saving the position...
      pos = source.pos
      source.next()

      // ...then reading all the text that follows, until the next double-quote
      val text = readEverythingUntil('"')

      if (source.ch == '"') {
        source.next(); // Consuming the final double-quote

        return Token(STRINGLITERAL(text)).setPosition(pos)
      } else {
        // Reached end-of-file : this is wrong!
        isEOF = true
        error(" - Unexpected EOF, expecting double-quote after that position", pos)
        return Token(BAD).setPosition(pos)
      }
    }

    // INTEGER LITERALS ===================================
    if (source.ch >= '0' && source.ch <= '9') {
      if (source.ch == '0') {
        // Started with zero ? It has to be a null integer
        // (no leading zeros accepted in Otter language)
        source.next()
        return Token(INTEGERLITERAL(0)).setPosition(source.pos)
      } else {
        // It's a regular integer, let's read it
        pos = source.pos
        // Saving starting digit position
        val integer = readInteger()
        return Token(INTEGERLITERAL(integer)).setPosition(pos)
      }
    }

    // OTHER TOKENS =======================================

    pos = source.pos

    token = source.ch match {
      case '(' => Token(OPAREN)
      case ')' => Token(CPAREN)
      case '[' => Token(OBRACKET)
      case ']' => Token(CBRACKET)
      case '{' => Token(OBLOCK)
      case '}' => Token(CBLOCK)
      case '+' => Token(PLUS)
      case '-' => Token(MINUS)
      case '*' => Token(MUL)
      case ',' => Token(COMMA)
      case ':' => Token(COLON)
      case ';' => Token(SEMICOLON)
      case '.' => Token(DOT)
      case '!' => Token(BANG)
      case '<' => Token(LESS)

      case '&' =>
        source.next()
        if (source.ch == '&') {
          Token(AND)
        } else {
          error("\n - Unexpected \"&\" characters, expecting \"&&\"", source.pos)
          // Returning immediately, to not consume the following character (readaheading)
          return Token(BAD).setPosition(source.pos)
        }

      case '|' =>
        source.next()
        if (source.ch == '|') {
          Token(OR)
        } else {
          error("\n - Unexpected \"|\" characters, expecting \"||\"", source.pos)
          // Returning immediately, to not consume the following character (readaheading)
          return Token(BAD).setPosition(source.pos)
        }

      case '=' =>
        source.next()
        if (source.ch == '=') {
          // Two equals signs ?
          Token(EQUALS)
        } else if (source.ch == '>') {
          Token(ARROW)
        } else {
          // Only on equals signs ?
          // Returning immediately, to not consume the following character (readaheading)
          return Token(ASSIGN).setPosition(pos)
        }

      case '/' =>
        source.next()

        if (source.ch == '/' || source.ch == '*') {
          // Handling
          if (source.ch == '/') {
            // Trimming content of the comments until the new line
            while (source.hasNext && source.ch != '\n')
              source.next()
          } else {
            // Trimming content of the comments until "*/"
            var previous = source.ch
            source.next()
            while (source.hasNext && !(previous == '*' && source.ch == '/')) {
              previous = source.ch
              source.next()
            }

            if (!source.hasNext) {
              if (previous == '*' && source.ch == '/') {
                // Multiline comment was the last thing of file (correctly closed)
                return Token(EOF).setPosition(source.pos)
              } else {
                // Un-terminated multiline comment
                error("\n - Unexpected EOF, expecting \"*/\"")
                return Token(BAD).setPosition(source.pos)
              }
            }
          }

          if (source.hasNext) source.next(); // Consume the \n or / (in */)

          return readNewToken(); // Find another token
        } else {
          return Token(DIV).setPosition(source.pos - 1)
        }

      case _ =>
        if (!source.hasNext && source.ch.isWhitespace) {
          // Handling edge case, when the last character is a whitespace
          // that wasn't dropped (we never drop the last char)

          Token(EOF)
        } else {
          // An unexpected character appears. Let's print an error message

          error("\n - Unexpected character : " + source.ch, source.pos)
          Token(BAD)
        }
    }

    if (source.hasNext) {
      source.next()
    } else {
      isEOF = true
    }

    token.setPosition(pos)

    token
  }

  /**
   * Works like an iterator, and returns the next token from the input stream. 
   */
  def nextToken: Token = {
    // Fixing position (first next)
    if (source.pos == 0)
      source.next()

    readNewToken()
  }
}
