package day4

import java.util.{Calendar, Date}

import util.day4.{Guard, LineLog}

import scala.annotation.tailrec
import scala.io.Source

object Part2 extends App {
  val t0 = System.nanoTime()

  val fileStream = getClass.getResourceAsStream("/inputDay4.txt")
  val lines = Source.fromInputStream(fileStream).getLines.toSeq



  val linesSorted: Seq[LineLog] = lines.map(x => LineLog(x)).sortBy(x => x.date)

  linesSorted foreach(x => println(x))

  val mapGuardsInfo: (Map[String, Map[String, Int]], Map[String, Int]) =  getTimesGuardList(linesSorted)
  val mapGuardTotalMinutes: Map[String, Int] = mapGuardsInfo._2
  val maxTimeGuard: (String, Int) = mapGuardTotalMinutes.maxBy(_._2)

  val mapFrequenciesMaxGuard: Seq[(Int, Int)] = mapGuardsInfo._1(maxTimeGuard._1).toSeq.map(x => (x._1.toInt, x._2)).sortBy(_._1)
  val mostCommonMinute= mapGuardsInfo._1(maxTimeGuard._1).maxBy(_._2)._1

  val result: (String, (String, Int)) = mapGuardsInfo._1.map(x => (x._1, x._2.maxBy(_._2))).maxBy(_._2._2)
  println(result)
  val id:Int = result._1.substring(1).toInt
  val minute = result._2._1.toInt
  println(s"id $id minute $minute")
  println(s"Solution ${id * minute}")


  def getTimesGuardList(lines: Seq[LineLog]): (Map[String, Map[String, Int]],Map[String, Int]) = {
    @tailrec
    def loop(lines: Seq[LineLog], mapGuardsFrequency: Map[String, Map[String, Int]], guardId: String, dateFalls: Option[Date], mapSleepTotalGuards: Map[String, Int]) : (Map[String, Map[String, Int]],Map[String, Int]) = {
      if (lines.isEmpty) (mapGuardsFrequency, mapSleepTotalGuards)
      else if (lines.head.message.startsWith("falls")){
        loop(lines.tail, mapGuardsFrequency, guardId, Some(lines.head.date), mapSleepTotalGuards)
      }
      else if (lines.head.message.startsWith("wakes")) {
        // get duration sleep
        val durationTime = getDurationTime(dateFalls.get, lines.head.date)
        // update map with max duration sleep time
        loop(
          lines.tail,
          updateMapFrequencies(mapGuardsFrequency, guardId, durationTime -1, dateFalls.get),
          guardId,
          dateFalls,
          updateMap(mapSleepTotalGuards, guardId, durationTime)
        )
      }
      else  {
        loop(lines.tail, mapGuardsFrequency, Guard.getGuardIdFromLine(lines.head), None, mapSleepTotalGuards)
      }
    }

    loop(lines, Map.empty, Guard.getGuardIdFromLine(lines.head), None, Map.empty)
  }

  def getDurationTime(fallDate: Date, wakeDate: Date): Int = {
    val diff = wakeDate.getTime - fallDate.getTime
    (diff / (60 * 1000) % 60).toInt

  }

  @tailrec
  def updateMapFrequencies(map: Map[String, Map[String, Int]], id: String, amountTime: Int, dateFall: Date): Map[String, Map[String, Int]] = {
    if (amountTime <= -1){
      map
    }else{
      val calendar = Calendar.getInstance
      calendar.setTime(dateFall)
      calendar.add(Calendar.MINUTE, amountTime)
      val minuteFrequency = calendar.get(Calendar.MINUTE)

      if (map.contains(id) && map(id).contains(minuteFrequency.toString)){
        val frequencies: Map[String, Int] = map(id) + (minuteFrequency.toString -> (map(id)(minuteFrequency.toString) + 1))
        val result = map + (id -> frequencies)
        updateMapFrequencies(result, id, amountTime - 1, dateFall)
      }else if (map.contains(id) && !map(id).contains(minuteFrequency.toString)){
        val frequencies: Map[String, Int] = map(id) + (minuteFrequency.toString -> 1)
        val result = map + (id -> frequencies)
        updateMapFrequencies(result, id, amountTime - 1, dateFall)
      }else{
        val result = map + (id -> Map(minuteFrequency.toString -> 1))
        updateMapFrequencies(result, id, amountTime - 1, dateFall)
      }
    }

  }

  def updateMap(map: Map[String, Int], id: String, valueToSave: Int): Map[String, Int] = {
    if (map.contains(id)) map + (id -> (valueToSave+map(id)))
    else map + (id -> valueToSave)
  }

  val t1 = System.nanoTime()
  println("\nElapsed time: " + (t1 - t0) + "ns")
}
