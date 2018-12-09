package day3

import util.LineInfo

import scala.io.Source

object Part2 extends App {


  case class Mark(value: String, oldValues: List[Int]){
    override def toString: String = value
  }

  val fileStream = getClass.getResourceAsStream("/inputDay3.txt")
  val lines: Iterator[String] = Source.fromInputStream(fileStream).getLines

  var count = 0

  val defaultValue = Mark("| |", List.empty)
  val markValue = Mark("|1|", List.empty)
  val doubleMarkedValue = Mark("|X|", List.empty)

  val rows = 1000

  val cols = 1000

  val a = Array.ofDim[Mark](rows, cols)

  for {
     i <- 0 until rows
     j <- 0 until cols
    } {
    a(i)(j) = defaultValue
  }

  val infoLinesList: Seq[LineInfo] =  lines.toSeq.map(x => LineInfo(x))

  infoLinesList.foreach(printClaim)

  a.foreach(row =>{
    println()
    row.foreach(x =>{
      print(x)
      if (x.value == doubleMarkedValue.value) count = count+1
    })
  })

  println(s"\nsquare inches of fabric are within two or more claims $count")

  val claimList: Seq[Int] = a.flatMap(row => {
    row.map(x => {
      x.oldValues
    })
  }).filter(x => x.length < 2).toList.flatten



  val goodClaim = infoLinesList
    .filter(x =>{x.size(0) * x.size(1) == claimList.count(c => c == x.id )})
    .take(1)

  println(s"the good claim Id is ${goodClaim.head.id}")


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
          a(i)(j) =  Mark(doubleMarkedValue.value, a(i)(j).oldValues :+ info.id)
        }
        else{
          a(i)(j) = Mark(markValue.value, List(info.id))
        }
      }
    }
  }

}
