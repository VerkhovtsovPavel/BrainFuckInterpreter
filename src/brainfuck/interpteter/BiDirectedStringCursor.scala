package brainfuck.interpteter

class BiDirectedStringCursor(val string: String) extends {

  var position = 0

  def hasNext: Boolean = string.length > position

  def hasPrevious: Boolean = position != 0

  def next(): Unit = position += 1

  def previous(): Unit = position -= 1

  def reset(): Unit = position = 0

  def current = string(position)

}
