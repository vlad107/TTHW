/**
  * Created by vlad107 on 16.05.16.
  */

import scala.io.Source
import java.io._

class HW5(inputFile: String, outputFile: String) {
  val bw = new BufferedWriter(new FileWriter(new File(outputFile)))
  bw.write(new BuildUnification(Source.fromFile(inputFile).getLines.toList
    .map(x => parseTermExpression(x))).toString)
  bw.close()
}
