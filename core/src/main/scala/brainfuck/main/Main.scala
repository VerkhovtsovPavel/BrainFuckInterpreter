package brainfuck.main

import scala.io.Source
import brainfuck.interpteter.BrainFuckInterpreter._

object Main extends App {
  val usage = """Usage: bfi [-h] [--help] [-f] [--file] source"""

  args match {
      case Array("-f" | "--file" ,fileName) => eval(Source.fromFile(fileName).mkString)
      case Array("-h" | "--help")           => println(usage);
      case Array(source)                    => eval(source)
      case _                                => println("Invalid command\n"+usage)
  }
}