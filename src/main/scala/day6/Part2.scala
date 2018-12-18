package day6

import java.io.InputStream

import scala.io.Source


object Part2 {
  val t0: Long = System.currentTimeMillis()
  val fileStream: InputStream = getClass.getResourceAsStream("/inputDay6.txt")

  val lines: Seq[String] = Source.fromInputStream(fileStream).getLines.toSeq
  val marks: Seq[String] = (0 to lines.length).map(String.valueOf)

  val maxValueCoordinate: Int = lines.flatten(x =>
    x.replaceAll(" ", "")
      .split(",")
      .map(Integer.parseInt))
    .max

  val cols: Int = maxValueCoordinate+1
  val rows: Int = maxValueCoordinate+1

  val a: Array[Array[String]] = Array.ofDim[String](rows, cols)

  for {
    i <- 0 until rows
    j <- 0 until cols
  } {
    a(i)(j) = "."
  }


  def main(args: Array[String]): Unit = {

    val criticPoint: Seq[(String, (Int, Int))] = marks zip lines.map(x => {
      val arrayCoor = x.replaceAll(" ","").split(",").map(Integer.parseInt)
      (arrayCoor(0), arrayCoor(1))
    })

    for {
      i <- 0 until rows
      j <- 0 until cols
    } yield {
      val z = for {
        x <- criticPoint
      } yield {
         val yy = Math.abs(x._2._1 - j) + Math.abs(x._2._2 - i)
        (x._1, yy)
      }
      val sumCoords = z.map(_._2).sum
      if (sumCoords < 10000) a(i)(j) = "*"
    }


    val solution = a.toSeq.flatten.count(_ == "*")
    println(s"Solution = $solution")


    val t1 = System.currentTimeMillis()
    println(s"\nElapsed time: ${t1 - t0} milliseconds")
  }



  def printTable():Unit = {
    a.foreach(row =>{
      println()
      row.foreach(x =>{
        print(x)
      })
    })
  }
}
