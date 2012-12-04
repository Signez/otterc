package toolc

object Main {
  def main(args: Array[String]) {
    if (args.length < 1) {
      Console.err.println("usage: toolc <File.tool>")
      sys.exit(1)
    }

    val compUnit = new Compiler(args(0))
    if(args.length == 1) {
      compUnit.compile(".")
    } else {
      if(!"-d".equals(args(1))) {
        compUnit.fatalError("Unrecognized option: " + args(1))
        Console.err.println("usage: toolc <File.tool>")
      }
      compUnit.compile(args(2))
    }
  }
}
