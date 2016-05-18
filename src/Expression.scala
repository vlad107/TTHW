/**
  * Created by vlad107 on 16.05.16.
  */
sealed trait Expression {
  def getFreeVariables:Set[String]
  def substitute(varName: String, expr: Expression):Expression = substitute(varName, expr, Set())
  def substitute(varName: String, expr: Expression, bound: Set[String]):Expression
  def normalize(): Expression = normalize(scala.collection.mutable.Map[String, Expression]())
  def normalize(cache: scala.collection.mutable.Map[String, Expression], untilNotLambda: Boolean = false): Expression
}

case class VarExpr(name: String) extends Expression {
  override def toString = name
  override def getFreeVariables = Set(name)

  override def substitute(varName: String, expr: Expression, bound: Set[String]): Expression = {
    if (varName.equals(name)) {
      val inter = bound & expr.getFreeVariables
//      if (inter.nonEmpty) throw new SubstitutionException(inter.head)
      expr
    } else new VarExpr(name)
  }

  override def normalize(cache: scala.collection.mutable.Map[String, Expression], untilNotLambda: Boolean = false): Expression = {
    if (!cache.contains(toString)) cache.put(name, new VarExpr(name))
    cache.get(name).get
  }
}

case class AppExpr(func: Expression, arg: Expression) extends Expression {
  override def toString = "(" + func.toString + " " + arg.toString + ")"
  override def getFreeVariables = func.getFreeVariables ++ arg.getFreeVariables

  override def substitute(varName: String, expr: Expression, bound: Set[String]): Expression
                                                                    = new AppExpr(func.substitute(varName, expr, bound),
                                                                                  arg.substitute(varName, expr, bound))

  override def normalize(cache: scala.collection.mutable.Map[String, Expression], untilNotLambda: Boolean = false): Expression = {
    val name = toString
    if (!cache.contains(name)) {
      val newFunc = func.normalize(cache, untilNotLambda = true)
      val newExpr = newFunc match {
        case lambda: LambdaExpr => lambda.apply(arg).normalize(cache, untilNotLambda)
        case _ => new AppExpr(newFunc.normalize(cache), if (untilNotLambda) arg else arg.normalize(cache))
      }
      cache += (name -> newExpr)
    }
    cache.get(name).get
  }
}

case class LambdaExpr(param: String, func: Expression) extends Expression {
  override def toString = "(" + "\\" + param + "." + func + ")"
  override def getFreeVariables = func.getFreeVariables - param

  override def substitute(varName: String, expr: Expression, bound: Set[String]): Expression = {
    if (varName.equals(param)) new LambdaExpr(param, func) else new LambdaExpr(param, func.substitute(varName, expr, bound + param))
  }


  override def normalize(cache: scala.collection.mutable.Map[String, Expression], untilNotLambda: Boolean = false): Expression = {
    val name = toString
    if (!cache.contains(name)) cache += (name -> (if (untilNotLambda) this else new LambdaExpr(param, func.normalize(cache))))
    cache.get(name).get
  }

  def apply(arg: Expression): Expression = func.substitute(param, arg)
}
