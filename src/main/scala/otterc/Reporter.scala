package otterc

/**
 * Reports warnings, errors, and fatal errors to user.
 *
 * This trait will be mixed into Compiler class and these methods will be called
 * anywhere in Analyzer, Expander, etc.
 */
trait Reporter {
  this: Compiler =>

  /**
   * Saves if compiler should stop its work at the end of current compiler step.
   */
  private var foundErrors = false

  /**
   * Output a simple warning.
   *
   * @param msg Message shown to compiler user.
   */
  def warn(msg: String): Unit =
    outputErrorMsg("Warning", msg)

  /**
   * Output a simple warning with position.
   *
   * @param msg Message shown to compiler user.
   * @param position Position where the problem was found.
   */
  def warn(msg: String, position: Int): Unit =
    if (position != 0) {
      report(position, "Warning: " + msg)
    } else {
      warn(msg)
    }

  /**
   * Output a simple warning with positional element.
   *
   * @param msg Message shown to compiler user.
   * @param pos Positional element that is problematic.
   */
  def warn(msg: String, pos: Positional): Unit =
    warn(msg, pos.position)

  /**
   * Report an error.
   *
   * A simple error will stop the process at the end of the current compiler step.
   *
   * @param msg Message shown to compiler user.
   */
  def error(msg: String): Unit = {
    foundErrors = true
    outputErrorMsg("Error", msg)
  }

  /**
   * Report an error.
   *
   * A simple error will stop the process at the end of the current compiler step.
   *
   * @param msg Message shown to compiler user.
   * @param position Position where the problem was found.
   */
  def error(msg: String, position: Int): Unit =
    if (position != 0) {
      foundErrors = true
      report(position, "Error: " + msg)
    } else {
      error(msg)
    }

  /**
   * Report an error.
   *
   * A simple error will stop the process at the end of the current compiler step.
   *
   * @param msg Message shown to compiler user.
   * @param pos Positional element that is problematic.
   */
  def error(msg: String, pos: Positional): Unit =
    error(msg, pos.position)

  /**
   * Report an error and stop immediately the compiling process.
   *
   * @param msg Message shown to compiler user.
   */
  def fatalError(msg: String): Nothing = {
    outputErrorMsg("Fatal error", msg); terminate
  }

  /**
   * Report an error and stop immediately the compiling process.
   *
   * @param msg Message shown to compiler user.
   * @param position Position where the problem was found.
   */
  def fatalError(msg: String, position: Int): Nothing = if (position != 0) {
    report(position, "Fatal error: " + msg)
    terminate
  } else {
    fatalError(msg)
  }

  /**
   * Report an error and stop immediately the compiling process.
   *
   * @param msg Message shown to compiler user.
   * @param pos Positional element that is problematic.
   */
  def fatalError(msg: String, pos: Positional): Nothing =
    fatalError(msg, pos.position)

  /**
   * Stops the process immediately if they were any errors (fatal or non-fatal).
   */
  def terminateIfErrors() : Unit = {
    if (foundErrors) {
      println("There were errors.")
      terminate
    }
  }

  /**
   * Output a message shown to compiler user.
   * 
   * @param prefix Prefix prepended to warning or error message.
   * @param message Warning or error message.
   */
  private def outputErrorMsg(prefix: String, message: String) : Unit = {
    println(prefix + ": " + message)
  }

  /**
   * Shutdown current process.
   */
  private def terminate: Nothing = {
    sys.exit(1)
  }

  import scala.io.Source

  private val sourceCopy: Source = Source.fromFile(fileName).withPositioning(on = true)
  private val sourceLines: IndexedSeq[String] = sourceCopy.getLines().toIndexedSeq
  sourceCopy.close()

  /**
   * Shows a warning or error message pointing invalid source.
   *
   * @param pos Masked position (see scala.io.Position).
   * @param msg Error or warning message shown to final user.
   */
  private def report(pos: Int, msg: String): Unit = {
    val buf = new StringBuilder
    val line = scala.io.Position.line(pos)
    val col = scala.io.Position.column(pos)
    buf.append(line + ":" + col + ": " + msg)
    buf.append("\n")
    buf.append(sourceLines(Math.max(line, 1) - 1))
    buf.append("\n")
    var i = 1
    while (i < col) {
      buf.append(' ')
      i += 1
    }
    buf.append('^')
    println(buf.toString())
  }
}
