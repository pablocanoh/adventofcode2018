package day2

import scala.io.Source

object Part2 extends App {

  val fileStream = getClass.getResourceAsStream("/inputDay2.txt")
  val lines: Seq[String] = Source.fromInputStream(fileStream).getLines.toSeq

  val listAllCombinations: Seq[Seq[String]] = (0 to 25).map(x => lines.map(y => new StringBuilder(y).deleteCharAt(x).toString()))

  findDuplicate(listAllCombinations)

  def findDuplicate(list: Seq[Seq[String]]): Unit ={
    def loop(listWord: Seq[String]): Unit ={
      if (listWord.isEmpty) print("")
      else if (listWord.count(x => x == listWord.head) > 1){
        println(s"the WORD is: ${listWord.filter(x => x == listWord.head).head}")
      }
      else loop(listWord.tail)
    }
    list.foreach(x => loop(x))
  }


}
