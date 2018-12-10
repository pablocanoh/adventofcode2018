package util.day4

import java.text.SimpleDateFormat
import java.util.Date

case class LineLog(
             date: Date,
             message: String
             )


object LineLog {
  val formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm")

  def apply(line: String): LineLog ={
    val dateString = line.substring(1, 17)
    val message = line.substring(19)

    new LineLog(formatter.parse(dateString), message)
  }
}
