import scala.io.StdIn.readChar

object BrainFuckUtil {


  private class BiDirectedStringCursor(val string: String) extends {
    var position = 0

    def hasNext: Boolean = string.length > position

    def hasPrevious: Boolean = position != 0

    def next() {
      position += 1
    }

    def previous() {
      position -= 1
    }

    def current = string(position)
  }

  def eval(code: String): Unit = {
    var index: Int = 0
    val array: Array[Int] = new Array[Int](3000)
    val source = new BiDirectedStringCursor(code)
    while (source.hasNext) {
      source.current match {
        case '+' => array(index) += 1
        case '-' => array(index) -= 1
        case '>' => index += 1
        case '<' => index -= 1
        case '.' => print(array(index).toChar)
        case ',' => array(index) = readChar()
        case '[' if array(index) == 0 => goToEndOfBlock(source)
        case ']' if array(index) != 0 => goToStartOfBlock(source)
        case _ =>
      }
      source.next()
    }
  }

  private def goToEndOfBlock(source: BiDirectedStringCursor): Unit = {
    var cycleLevel = 1
    while (cycleLevel != 0) {
      source.next()
      source.current match {
        case '[' => cycleLevel += 1
        case ']' => cycleLevel -= 1
        case _ =>
      }
    }
  }

  private def goToStartOfBlock(source: BiDirectedStringCursor): Unit = {
    var cycleLevel = 1
    while (cycleLevel != 0) {
      source.previous()
      source.current match {
        case '[' => cycleLevel -= 1
        case ']' => cycleLevel += 1
        case _ =>
      }
    }
  }

  implicit class BrainFuckInterpreter(val sc: StringContext) extends AnyVal {
    def bf(codeBlocks: String*): Unit = {
      val strings = sc.parts.iterator
      val expressions = codeBlocks.iterator
      val buf = new StringBuffer(strings.next)
      while (strings.hasNext) {
        buf.append(expressions.next).append(strings.next)
      }

      eval(buf.toString)
    }
  }

}
