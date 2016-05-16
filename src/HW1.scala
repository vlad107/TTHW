/**
  * Created by vlad107 on 16.05.16.
  */

import scala.io.Source
import java.io._

class HW1(inputFile: String, outputFile: String) {
  val lambdaStrings = Source.fromFile(inputFile).getLines.toList
  val bw = new BufferedWriter(new FileWriter(new File(outputFile)))
  bw.write(lambdaStrings.map(x => parseLambdaExpression(x)).map(x => x.toString).mkString("\n"))
  bw.close()
}
