package util.day3

case class LineInfo(
                   id: Int,
                   startPosition: Array[Int],
                   size: Array[Int]
                   )

object LineInfo {

  def apply(line: String): LineInfo = {

    val positionPad = line.indexOf("#")
    val positionAt = line.indexOf("@")
    val positionTwoPoints = line.indexOf(":")

    val id: String = line.substring(positionPad+1, positionAt-1)
    val startPosition = line.substring(positionAt+2, positionTwoPoints)
    val size = line.substring(positionTwoPoints+2)

    new LineInfo(
      Integer.parseInt(id),
      startPosition.split(",").map(x => Integer.parseInt(x)),
      size.split("x").map(x => Integer.parseInt(x))
    )
  }

}


