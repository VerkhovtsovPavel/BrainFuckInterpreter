package brainfuck.interpteter

case class BrainFuckSyntaxException(message: String) extends Exception(message)