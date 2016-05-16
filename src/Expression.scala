/**
  * Created by vlad107 on 16.05.16.
  */
sealed trait Expression {
  def getFreeVariables:Set[String]
  def substitute(varName: String, expr: Expression):Expression = {
    substitute(varName, expr, Set())
  }
  def substitute(varName: String, expr: Expression, bound: Set[String]):Expression
}

case class VarExpr(name: String) extends Expression {
  override def toString = name
  override def getFreeVariables = Set(name)

  override def substitute(varName: String, expr: Expression, bound: Set[String]): Expression = {
    if (varName.equals(name)) {
      val inter = bound & expr.getFreeVariables
      if (inter.nonEmpty) throw new SubstitutionException(inter.head)
      expr
    } else new VarExpr(name)
  }
}

case class AppExpr(func: Expression, arg: Expression) extends Expression {
  override def toString = "(" + func.toString + " " + arg.toString + ")"
  override def getFreeVariables = func.getFreeVariables ++ arg.getFreeVariables

  override def substitute(varName: String, expr: Expression, bound: Set[String]): Expression
                                                                    = new AppExpr(func.substitute(varName, expr, bound),
                                                                                  arg.substitute(varName, expr, bound))
}

case class LambdaExpr(param: String, func: Expression) extends Expression {
  override def toString = "(" + "\\" + param + "." + func + ")"
  override def getFreeVariables = func.getFreeVariables - param

  override def substitute(varName: String, expr: Expression, bound: Set[String]): Expression
                                                                    = new LambdaExpr(param,
                                                                      func.substitute(varName, expr, bound + param))
}
