package day5

import scala.annotation.tailrec
import scala.io.Source

object Part1 extends App {
  val t0 = System.currentTimeMillis()
  val fileStream = getClass.getResourceAsStream("/inputDay5.txt")
  val lines: Seq[String] = Source.fromInputStream(fileStream).getLines.toSeq

  println(countUnits(lines))



  def countUnits(lines: Seq[String]): Int = {
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

    lines.map(x => loopCountUnits(x, 0).length).sum

  }

  val t1 = System.currentTimeMillis()
  println(s"\nElapsed time: ${t1 - t0} milliseconds")
}
