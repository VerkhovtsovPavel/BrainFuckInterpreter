package compileTimeChecking

import scala.reflect.macros._
import scala.language.experimental.macros
object CompileTimeSyntaxChecking {

  def checkSyntaxImpl(c: blackbox.Context)(code : c.Expr[String]) : c.Tree = {
    import c.universe._

    if(!_checkSyntax(code.tree.toString()))
      c.abort(c.enclosingPosition, "Syntax error in bf code")
    else
      code.tree
  }

  private def _checkSyntax(code: String): Boolean = {
    val source = code.iterator
    val blockStack = new scala.collection.mutable.Stack[Char]()
    var result : Boolean = true
    while (source.hasNext) {
      source.next() match {
        case '[' => blockStack.push('[')
        case ']' if blockStack.isEmpty => result &= false
        case ']' if blockStack.top != '[' => result &= false
        case ']' => blockStack.pop()
        case _ =>
      }
    }
    if (blockStack.nonEmpty || code.replaceAll("""[\+\-\[\<\>\.\]\,]""","").length > 0)
      result &= false

    result
  }

  def checkSyntax(code : String) : Unit = macro checkSyntaxImpl
}
