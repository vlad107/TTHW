/**
  * Created by vlad107 on 16.05.16.
  */

import scala.io.Source
import java.io._

class HW3(inputFile: String, outputFile: String) {
  val bw = new BufferedWriter(new FileWriter(new File(outputFile)))
  Source.fromFile(inputFile).getLines
    .map(x => parseLambdaExpression(x))
    .map(x => x.getFreeVariables)
    .map(x => collection.immutable.SortedSet[String]() ++ x)
    .foreach(x => x.foreach(y => bw.write(y + "\n")))
  bw.close()
}
