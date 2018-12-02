package day1

import scala.annotation.tailrec
import scala.io.Source

object part2 extends App {

  println(readFile())

  def readFile(): Int = {

    val fileStream = getClass.getResourceAsStream("/inputDay1.txt")
    val numbers = Source.fromInputStream(fileStream).getLines.map(x => Integer.parseInt(x)).toList

    @tailrec
    def loop(solution: Option[Int] = None, accumulator: Int= 0, pastNumbers: Seq[Int], listNumbers: List[Int]): Int = {
      if (listNumbers.isEmpty && solution.isDefined){
        solution.get
      } else{
        if (listNumbers.isEmpty && solution.isEmpty){
          loop(None, accumulator, pastNumbers, numbers)
        }else{
          val finalAcumulator = accumulator + listNumbers.head
          if (pastNumbers.contains(finalAcumulator)) loop(Some(finalAcumulator), finalAcumulator, pastNumbers, List.empty)
          else loop(None, finalAcumulator, pastNumbers :+ finalAcumulator, listNumbers.tail)
        }
      }
    }
    loop(pastNumbers = Seq(0), listNumbers = numbers)
  }

}