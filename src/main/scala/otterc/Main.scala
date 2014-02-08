package otterc

object Main {
  /**
   * Main entry point for Otter Compiler.
   *
   * @param args Arguments from command-line.
   */
  def main(args: Array[String]) {

    // Not enough arguments? Printing usage
    if (args.length != 1 && args.length != 3) {
      Console.err.println("usage: otterc <File.otr>")
      Console.err.println("       otterc -d <output-directory> <File.otr>")
      sys.exit(1)
    }

    if (args.length == 1) {
      val compUnit = new Compiler(args(0))
      compUnit.compile("./")
    } else if (args.length == 3) {
      val compUnit = new Compiler(args(2))

      if (!"-d".equals(args(0))) {
        compUnit.fatalError("Unrecognized option: " + args(0))
      }

      compUnit.compile(args(1))
    }
  }
}
