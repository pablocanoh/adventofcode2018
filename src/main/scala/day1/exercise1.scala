package day1

import scala.io.Source

object exercise1 extends App {

  val fileStream = getClass.getResourceAsStream("/inputDay1.txt")
  val lines = Source.fromInputStream(fileStream).getLines
  val finalSolution = lines.map(x => Integer.parseInt(x)).sum
  println(finalSolution)

}
