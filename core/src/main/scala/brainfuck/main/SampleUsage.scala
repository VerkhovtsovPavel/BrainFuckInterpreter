package main

import brainfuck.interpteter.BrainFuckInterpreter._

import scala.io.Source

object SampleUsage {
  bf"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.+++++++++++++++++++++++++++++.+++++++..+++.-------------------------------------------------------------------------------.+++++++++++++++++++++++++++++++++++++++++++++++++++++++.++++++++++++++++++++++++.+++.------.--------.-------------------------------------------------------------------.-----------------------."

  val block = "[>+++++++>++++++++++>+++>+<<<<-]"
  bf"++++++++++$block>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

  eval(Source.fromFile("./sample/helloWorld1.b").mkString)

  compileTimeChecking.CompileTimeSyntaxChecking.checkSyntax("++++++++++>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.");
}
