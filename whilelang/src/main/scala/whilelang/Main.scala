package whilelang

import parser.Parser

object Main {
  def main(args : Array[String]) : Unit = {
    if(args.length < 1) {
      Console.err.println("Error: please provide a program to interpret.")
      exit(-1)
    }

    import java.io.{FileInputStream,IOException}

    val interpreter = new Interpreter
    
    for(fileName <- args) {
      try {
        val in = new FileInputStream(fileName)
        println("Source of " + fileName + ":")
        val parsed = (new Parser).parseInputStream(in)
        TreePrinter(parsed)
        println("Running interpreter")
        interpreter.run(parsed)

        println("Simplified source of " + fileName + ":")
        val converted = TreeSimplifier(parsed)
        println("Running interpreter again")
        interpreter.run(converted)
      } catch {
        case e: IOException => error(e.getMessage)
      }
    }
  }
}
