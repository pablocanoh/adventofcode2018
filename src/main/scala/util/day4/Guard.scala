package util.day4

case class Guard(id: String)

object Guard {

  def getGuardIdFromLine(log: LineLog): String = log.message.split(" ")(1)

}

