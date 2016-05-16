/**
  * Created by vlad107 on 16.05.16.
  */

import scala.io.Source
import java.io._

class HW4(inputFile: String, outputFile: String) {
  val bw = new BufferedWriter(new FileWriter(new File(outputFile)))
  Source.fromFile(inputFile).getLines
    .map(x => parseLambdaExpression(x).normalize())
    .foreach(x => bw.write(x + "\n"))
  bw.close()
}
