package challenge

import java.time.LocalTime
import scala.io.Source;

object OffenseParser {
  def parse(path: String): Seq[Offense] = {
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
    }).filter(_.nonEmpty)
      .map(_.get)

    offenses
  }
}
