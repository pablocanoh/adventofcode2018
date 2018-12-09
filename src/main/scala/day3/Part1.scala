package day3

import util.LineInfo

import scala.io.Source

object Part1 extends App {

  val fileStream = getClass.getResourceAsStream("/inputDay3.txt")
  val lines: Iterator[String] = Source.fromInputStream(fileStream).getLines



  var count = 0

  val defaultValue = "| |"

  val rows = 1000

  val cols = 1000

  val a = Array.ofDim[String](rows, cols)

  for {
     i <- 0 until rows
     j <- 0 until cols
    } {
    a(i)(j) = defaultValue
  }

  lines.map(x => LineInfo(x)).foreach(printClaim)

  a.foreach(row =>{
    println()
    row.foreach(x =>{
      print(x)
      if (x == "|X|") count = count+1
    })
  }

    )

  println()
  println(count)

  def printClaim(info: LineInfo): Unit = {
    val leftEdge:Int = info.startPosition(0)
    val topEdge:Int = info.startPosition(1)

    val width:Int = info.size(0)
    val height:Int = info.size(1)

    for {
      i <- 0 until rows
      j <- 0 until cols
    } {
      if (
        i >= topEdge && i < topEdge+height
          && j >= leftEdge && j < leftEdge+width
      ){
        if (a(i)(j) != defaultValue){
          a(i)(j) = "|X|"
        }
        else a(i)(j) = String.valueOf(s"|1|")
      }
    }
  }

}
