package day5

import scala.annotation.tailrec
import scala.io.Source

object Part2 extends App {
  val t0 = System.currentTimeMillis()
  val fileStream = getClass.getResourceAsStream("/inputDay5.txt")
  val line: String = Source.fromInputStream(fileStream).getLines.toSeq.head

  val letters: Seq[Char] = line.map(x => x.toUpper).distinct

  val linesCombinationsRemove: Seq[(Char, String)] = letters.map(x => (x, line.filterNot(y => y == x.toUpper || y == x.toLower)))


  println(countUnits(linesCombinationsRemove))


  def countUnits(lines: Seq[(Char,String)]): (Char, Int) = {
    @tailrec
    def loopCountUnits(line: String, controlLoop: Int): String ={
      if (controlLoop+1 == line.length) line
      else if(line.charAt(controlLoop).isLower && line.charAt(controlLoop+1).isUpper &&
        line.charAt(controlLoop).toUpper == line.charAt(controlLoop+1).toUpper){
        val sb = new StringBuilder(line)
        sb.delete(controlLoop, controlLoop+2)
        loopCountUnits(sb.toString(), 0)
      }
      else if(line.charAt(controlLoop).isUpper && line.charAt(controlLoop+1).isLower &&
        line.charAt(controlLoop).toUpper == line.charAt(controlLoop+1).toUpper){
        val sb = new StringBuilder(line)
        sb.delete(controlLoop, controlLoop+2)
        loopCountUnits(sb.toString(), 0)
      }
      else{
        loopCountUnits(line, controlLoop+1)
      }
    }

    lines.map(x => (x._1, loopCountUnits(x._2, 0).length)).minBy(_._2)
  }

  val t1 = System.currentTimeMillis()
  println(s"\nElapsed time: ${t1 - t0} milliseconds")
}
