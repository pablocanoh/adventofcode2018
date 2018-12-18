package day6

import java.io.InputStream

import scala.io.Source


object Part1 {
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


  def main(args: Array[String]): Unit = {

    val criticPoint: Seq[(String, (Int, Int))] = marks zip lines.map(x => {
      val arrayCoor = x.replaceAll(" ","").split(",").map(Integer.parseInt)
      (arrayCoor(0), arrayCoor(1))
    })

    val sol: Seq[String] = (for {
      i <- 0 until rows
      j <- 0 until cols
    } yield {
      val z = for {
        x <- criticPoint
      } yield {
        val yy = Math.abs(x._2._1 - j) + Math.abs(x._2._2 - i)
        (x._1, yy)
      }
      val winner = z.minBy(_._2)
      if (z.count(_._2 == winner._2) <= 1) a(i)(j) = winner._1
      else a(i)(j) = "*"
      if (i == 0 || i == (maxValueCoordinate+1) || j == 0 || j == (maxValueCoordinate+1))
        winner._1
      else "*"

    }).distinct


    val dd: Seq[String] = a.toSeq.flatten.filterNot(sol.contains(_))
    val bestMark = dd.groupBy(identity).maxBy(_._2.size)._1
    println(s"best mark is $bestMark")
    println(s"Solution = ${dd.count(_ == bestMark)}")


    val t1 = System.currentTimeMillis()
    println(s"\nElapsed time: ${t1 - t0} milliseconds")
  }
}
