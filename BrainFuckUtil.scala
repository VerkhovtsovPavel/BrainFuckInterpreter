import scala.io.StdIn.readChar

object BrainFuckUtil {

  implicit class BrainFuckInterpreter(val sc: StringContext) extends AnyVal {
    def bf(args: Any*): Unit = {
      val code = sc.toString
      var cycleLevel = 0
      var index: Int = 0
      val array: Array[Int] = new Array[Int](3000)
      var position = 0
      while (position < code.length) {
        code(position) match {
          case '+' => array(index) += 1
          case '-' => array(index) -= 1
          case '>' => index += 1
          case '<' => index -= 1
          case '.' => print(array(index).toChar)
          case ',' => array(index) = readChar()
          case '[' if array(index) == 0 =>
            cycleLevel += 1
            while (cycleLevel != 0) {
              position += 1
              code(position) match {
                case '[' => cycleLevel += 1
                case ']' => cycleLevel -= 1
                case _ =>
              }
            }
          case ']' if array(index) != 0 =>
            cycleLevel += 1
            while (cycleLevel != 0) {
              position -= 1
              code(position) match {
                case '[' => cycleLevel -= 1
                case ']' => cycleLevel += 1
                case _ =>
              }
            }
            position -= 1
          case _ =>
        }
        position += 1
      }
    }
  }
}
