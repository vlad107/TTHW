import scala.collection.mutable

/**
  * Created by vlad107 on 17.05.16.
  */
class BuildUnification(eqs: List[(TermExpression, TermExpression)]) {
  var result = mutable.Map.empty[String, TermExpression]
  override def toString: String = result.toList.map{case (name, expr) => name + "=" + expr}.mkString("\n")
  var queue = mutable.Queue.empty[(TermExpression, TermExpression)]
  queue ++= eqs
  while (queue.nonEmpty) {
    val (term1, term2) = queue.dequeue
    (term1, term2) match {
      case (x: TermFunc, y: TermFunc) =>
        if (!x.name.equals(y.name)) throw new UnificationException
        queue ++= x.args zip y.args
      case (x: TermVar, y: TermExpression) =>
        result.get(x.name) match {
          case None =>
            var updatedY = y
            for ((p, v) <- result) updatedY = updatedY.substitute(p, v)
            if (updatedY.contains(x.name)) throw new UnificationException
            result.put(x.name, updatedY)
          case Some(z) => queue += Tuple2(y, z)
        }
      case (x: TermExpression, y: TermVar) => queue += Tuple2(y, x)
    }
  }
}
