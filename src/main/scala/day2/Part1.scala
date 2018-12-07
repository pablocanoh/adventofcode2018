package day2

import scala.io.Source

object Part1 extends App {

  val fileStream = getClass.getResourceAsStream("/inputDay2.txt")
  val lines: Iterator[String] = Source.fromInputStream(fileStream).getLines
  val listLines: List[Int] = lines.flatMap(x => deleteDuplicates(countLetters(x))).toList.filter(x => x > 1)

  System.out.println(sumLines(listLines))

  def countLetters(text: String): Map[String, Int] =
    text
      .split("")
      .map(_.toLowerCase)
      .groupBy(identity)
      .mapValues(_.length)

  def deleteDuplicates(map :Map[String, Int]): List[Int] ={
    map.values.toList.distinct
  }

  def sumLines(iterable: Iterable[Int]): Int = {
    def loop(iterable: Iterable[Int], map:Map[Int, Int]): Map[Int, Int] = {
      if (iterable.isEmpty){
        map
      } else if (map.contains(iterable.head)){
        loop(iterable.tail, map + (iterable.head -> (map(iterable.head)+1)))
      } else {
        loop(iterable.tail, map + (iterable.head -> 1))
      }
    }

    loop(iterable, Map.empty).values.product

  }

}




