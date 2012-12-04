package toolc

object Main {
  def main(args: Array[String]) {
    if (args.length != 1 && args.length != 3) {
      Console.err.println("usage: toolc <File.tool>")
      Console.err.println("       toolc -d <outdir> <File.tool>")
      sys.exit(1)
    }

    if (args.length == 1) {
      val compUnit = new Compiler(args(0))
      compUnit.compile("./")
    } else if (args.length == 3) {
      val compUnit = new Compiler(args(2))

      if(!"-d".equals(args(0))) {
        compUnit.fatalError("Unrecognized option: " + args(0))
      }

      compUnit.compile(args(1))
    }
  }
}
