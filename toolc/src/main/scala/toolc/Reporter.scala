package toolc

trait Reporter {
  // Informs scalac that this will be mixed-in with the class Compiler, so
  // we'll have access to the source file information (its file name).
  // We read the source file a second time here for displaying error messages.
  this: Compiler =>

  private var foundErrors = false
  
  /** Simple warnings */
  def warn(msg: String): Unit = outputErrorMsg("Warning", msg)
  def warn(msg: String, i: Int): Unit = if(i != 0) report(i, "Warning: " + msg) else warn(msg)
  def warn(msg: String, pos: Positional): Unit = warn(msg, pos.pos)
  
  /** Non-fatal errors. The compiler should call terminateIfErrors
   * before the errors can have an impact (for instance at the end
   * of the phase). */
  def error(msg: String): Unit = { foundErrors = true; outputErrorMsg("Error", msg) }
  def error(msg: String, i: Int): Unit = if(i != 0) { foundErrors = true; report(i, "Error: " + msg) } else error(msg)
  def error(msg: String, pos: Positional): Unit = error(msg, pos.pos)

  /** Errors from which the compiler cannot recover or continue. */
  def fatalError(msg: String): Nothing = { outputErrorMsg("Fatal error", msg); terminate }
  def fatalError(msg: String, i: Int): Nothing = if (i != 0) {
    report(i, "Fatal error: " + msg)
    terminate
  } else fatalError(msg)
  def fatalError(msg: String, pos: Positional): Nothing = fatalError(msg, pos.pos)
  
  /** Stops the compiler if they were non-fatal errors. */
  def terminateIfErrors = {
    if(foundErrors) {
      println("There were errors.")
      terminate
    }
  }

  private def outputErrorMsg(prefix: String, msg: String) = {
    println(prefix + ": " + msg)
  }
  
  private def terminate: Nothing = {
    sys.exit(1)
  }

  import scala.io.Source
  private val sourceCopy: Source = Source.fromFile(fileName).withPositioning(true)
  private val sourceLines: IndexedSeq[String]  = sourceCopy.getLines().toIndexedSeq
  sourceCopy.close()

  // circumvents what I think is a bug in scala.io.Source
  private def report(pos: Int, msg: String): Unit = {
    // sourceCopy.report(pos, msg, scala.Console.out)
    val buf = new StringBuilder
    val line = scala.io.Position.line(pos)
    val col = scala.io.Position.column(pos)
    buf.append(line + ":" + col + ": " + msg)
    buf.append("\n")
    buf.append(sourceLines(line-1))
    buf.append("\n")
    var i = 1
    while(i < col) {
      buf.append(' ')
      i += 1
    }
    buf.append('^')
    println(buf.toString)
  }
}
