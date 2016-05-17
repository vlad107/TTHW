
/**
  * Created by vlad107 on 16.05.16.
  */
class LambdaParserException(rem: String) extends Exception {
  override def getMessage: String = "Ошибка в парсере, остаток: " + rem
}
class TermParserException(rem: String) extends Exception {
  override def getMessage: String = "Ошибка в парсере, остаток: " + rem
}
class SubstitutionException(varName: String) extends Exception {
  override def getMessage: String = "Нет свободы для подстановки для переменной " + varName
}

class UnificationException() extends Exception {
  override def getMessage: String = "Система не разрешима"
}

object Solver {
  final val USAGE = "Usage: Solver <homework number> <input file> <output file>"

  def main(args: Array[String]): Unit = try {
    args(0) match {
      case "1" => new HW1(args(1), args(2))
      case "2" => new HW2(args(1), args(2))
      case "3" => new HW3(args(1), args(2))
      case "4" => new HW4(args(1), args(2))
      case "5" => new HW5(args(1), args(2))
      case "6" => println("Sixth task")
      case _ => throw new IndexOutOfBoundsException()
    }
  } catch {
    case _: IndexOutOfBoundsException => System.err.println(USAGE)
    case e: LambdaParserException => System.err.println(e.getMessage)
    case e: SubstitutionException => System.err.println(e.getMessage)
    case e: UnificationException => System.err.println(e.getMessage)
  }
}
