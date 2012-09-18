package toolc

object Main {
  def main(args: Array[String]) {
    if (args.length != 1) {
      Console.err.println("usage: toolc <File.tool>")
      sys.exit(1)
    }

    val compUnit = new Compiler(args(0))
    compUnit.compile
  }
}
