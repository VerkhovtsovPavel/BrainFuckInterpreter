package brainfuck

case class BrainFuckSyntaxException(message: String) extends Exception(message)