package challenge

import java.time.LocalTime
import java.time.LocalDate

import scala.io.Source;

object Challenge {
  case class Offense(station: String, line: String, day: String, reportStartTime: LocalTime, reportEndTime: LocalTime, description: String, offense: String,
    victimSex: String, victimAge: Int, offenderSex: String, offenderAge: Int, offenderDetained: Boolean)

  def main(args: Array[String]): Unit = {
    val path = args(0)
    val rawEvents: Seq[String] = Source.fromFile(path)
      .getLines
      .drop(1)
      .toSeq  

    val offenses: Seq[Offense] = rawEvents.map(event => {
      try {
        val offenseArray = event.split(",")
        offenseArray.toList match {
          case station :: line :: day :: reportStartTime :: reportEndTime :: description :: offense :: victimSex :: victimAge :: offenderSex :: offenderAge :: offenderDetained :: Nil =>
            val offen = Offense(station, line, day, LocalTime.parse(reportStartTime), LocalTime.parse(reportEndTime), description, offense, victimSex, victimAge.toInt, offenderSex, offenderAge.toInt, offenderDetained != "NO ARREST")
            Option(offen);
          case station :: line :: day :: reportStartTime :: reportEndTime :: description :: offense :: victimSex :: victimAge :: offenderSex :: offenderAge :: Nil =>
            val offen = Offense(station, line, day, LocalTime.parse(reportStartTime), LocalTime.parse(reportEndTime), description, offense, victimSex, victimAge.toInt, offenderSex, offenderAge.toInt, true)
            Option(offen)
        }
      } catch {
        case e => 
          Option.empty
      }
    })
      .filter(_.nonEmpty)
      .map(_.get)

    val order = Ordering.by[(String, Int), Int](_._2)(Ordering.Int.reverse)

    //question 1
    val top10 = offenses.groupBy(_.station)
      .map{case (station, offenses) => (station, offenses.size)}
      .toList
      .sorted(order)
      .take(10)

    // question 2
    val times = offenses.map(offense => offense.reportEndTime.minusHours(offense.reportStartTime.getHour).minusMinutes(offense.reportStartTime.getMinute))
      .map(_.toSecondOfDay)

    // question 3
    val meanWaitTime = LocalTime.ofSecondOfDay(times.sum / times.size)

    println(top10)
    println(meanWaitTime)

  }
}
