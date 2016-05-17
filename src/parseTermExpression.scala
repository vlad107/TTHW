import scala.collection.mutable._

/**
  * Created by vlad107 on 17.05.16.
  */
object parseTermExpression {
  def apply(tempStr: String): (TermExpression, TermExpression) = {
    val str = tempStr.replaceAll("\\s", "")
    var curPos = 0

    def passIfEq(c: Char): Boolean = {
      if ((curPos < str.length) && (str.charAt(curPos) == c)) {
        curPos += 1
        return true
      }
      false
    }

    def passOrThrow(c: Char) = if (!passIfEq(c)) throw new LambdaParserException(str.substring(curPos))

    def parseEq(): (TermExpression, TermExpression) = {
      val term1 = parseTerm()
      passOrThrow('=')
      val term2 = parseTerm()
      (term1, term2)
    }

    def parseTerm(): TermExpression = {
      val name = parseVar()
      if (passIfEq('(')) {
        val args = new ListBuffer[TermExpression]
        args += parseTerm()
        while (passIfEq(',')) args += parseTerm()
        passOrThrow(')')
        return new TermFunc(name, args.toList)
      }
      new TermVar(name)
    }

    def parseVar(): String = {
      if ((curPos >= str.length) || (!Character.isLetter(str.charAt(curPos)))) {
        throw new TermParserException(str.substring(curPos))
      }
      val oldPos = curPos
      while ((curPos < str.length) && (Character.isLetterOrDigit(str.charAt(curPos)) || str.charAt(curPos) == '\'')) curPos += 1
      str.substring(oldPos, curPos)
    }

    parseEq()
  }
}
