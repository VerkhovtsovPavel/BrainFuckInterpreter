package compileTimeChecking

import scala.reflect.macros._
import scala.language.experimental.macros
object CompileTimeSyntaxChecking {

  def checkSyntaxImpl(c: blackbox.Context)(code : c.Expr[String]) : c.Tree = {
    import c.universe._

     val res = _checkSyntax(code.tree.toString()) 

    if(res.nonEmpty)
      c.abort(c.enclosingPosition, "Syntax error in bf code : " + res)
    else
      code.tree
  }

  private def _checkSyntax(code: String): String = {

    val source = code.iterator
    val blockStack = new scala.collection.mutable.Stack[Char]()
    var result : String = ""
    while (source.hasNext) {
      source.next() match {
        case '[' => blockStack.push('[')
        case ']' if blockStack.isEmpty | blockStack.top != '[' => result = "Unvalid block sequеnce ']' before '['"
        case ']' => blockStack.pop()
        case _ =>
      }
    }
    if (blockStack.nonEmpty)
      result = "Unvalid block sequеnce ']' before '['"

    val invalidSymbols = code.replaceAll("""[\+\-\[\<\>\.\]\,\"]""","")
    if (invalidSymbols.nonEmpty)
      result = "Unvalid symbols "+invalidSymbols

    result
  }

  def checkSyntax(code : String) : Unit = macro checkSyntaxImpl
}
