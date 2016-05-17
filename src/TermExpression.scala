/**
  * Created by vlad107 on 17.05.16.
  */
sealed trait TermExpression {
  def substitute(name: String, expr: TermExpression): TermExpression
  def contains(name: String): Boolean
}

case class TermFunc(name: String, args: List[TermExpression]) extends TermExpression {
  override def substitute(name: String, expr: TermExpression) = new TermFunc(this.name, args.map(x => x.substitute(name, expr)))
  override def contains(name: String) = args.exists(x => x.contains(name))
  override def toString: String = name + "(" + args.map(x => x.toString).mkString(",") + ")"
}

case class TermVar(name: String) extends TermExpression {
  override def substitute(name: String, expr: TermExpression) = if (name.equals(this.name)) expr else this
  override def contains(name: String) = name.equals(this.name)
  override def toString: String = name
}
