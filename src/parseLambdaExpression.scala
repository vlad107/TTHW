import scala.util.parsing.combinator.RegexParsers

/**
  * Created by vlad107 on 16.05.16.
  */


object parseLambdaExpression {
  def apply(tmpStr: String): Expression = {
    val str = tmpStr.trim
    var curPos = 0

    def passIfEq(c: Char): Boolean = {
      if ((curPos < str.length) && (str.charAt(curPos) == c)) {
        curPos += 1
        return true
      }
      false
    }

    def passOrThrow(c: Char) = if (!passIfEq(c)) throw new LambdaParserException(str.substring(curPos))


    def parseExpr(): Expression = {
      if (curPos >= str.length) throw new LambdaParserException(str.substring(curPos))
      str.charAt(curPos) match {
        case '\\' => parseLambda()
        case _ =>
          var app = parseApp()
          if ((curPos < str.length) && (str.charAt(curPos) == '\\')) app = new AppExpr(app, parseLambda())
          app
      }
    }

    def parseApp(): Expression = {
      var result = parseAtom()
      while (passIfEq(' ') && ((curPos >= str.length) || (str.charAt(curPos) != '\\'))) {
        result = new AppExpr(result, parseAtom())
      }
      result
    }

    def parseAtom(): Expression = {
      if (passIfEq('(')) {
        val result = parseExpr()
        passOrThrow(')')
        return result
      }
      new VarExpr(parseVariable())
    }

    def parseLambda(): LambdaExpr = {
      passOrThrow('\\')
      val param = parseVariable()
      passOrThrow('.')
      val func = parseExpr()
      new LambdaExpr(param, func)
    }

    def parseVariable(): String = {
      if (curPos < str.length && Character.isLetter(str.charAt(curPos))) {
        val startPos = curPos
        while (curPos < str.length && Character.isLetterOrDigit(str.charAt(curPos))) curPos += 1
        return str.substring(startPos, curPos)
      }
      throw new LambdaParserException(str.substring(curPos))
    }

    var expr = parseExpr()
    if (passIfEq('[')) {
      val varName = parseVariable()
      passOrThrow(':')
      passOrThrow('=')
      expr = expr.substitute(varName, parseExpr())
      passOrThrow(']')
    }
    expr
  }
}
