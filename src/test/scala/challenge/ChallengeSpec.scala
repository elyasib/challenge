package challenge

import org.scalatest._
import scala.io.Source;
import Solutions._
import java.time.LocalTime

class HelloSpec extends FlatSpec with Matchers {
  val rawEvents: Seq[String] = Source.fromResource("data_.csv")
    .getLines
    .drop(1)
    .toSeq  

  "OffenseParser" should "parse offenses" in {
     val offenses = OffenseParser.parse(rawEvents)
     offenses.size should be > 0 
  }

  "Solution" should "should find the top10 stations most dangerous for woman" in {
    val offenses = OffenseParser.parse(rawEvents)
    val expected = List(("HIDALGO", 31), ("BALDERAS", 18), ("PINO SUÁREZ", 14), ("PANTITLÁN", 14), ("INDIOS VERDES", 13), ("TACUBAYA", 12),
      ("BELLAS ARTES", 10), ("GUERRERO", 9), ("SALTO DEL AGUA", 8), ("ZÓCALO", 7))
    val top10 = getTop10(offenses)

    println(s"\nTop10 stations most dangerous for woman:\n${top10.mkString(",")}")

    top10 === expected
  }

  "Solution" should "find the top female-offender age-group with the highest number of offenses" in {
    val offenses = OffenseParser.parse(rawEvents)
    val expected = ((31, 31), 1)
    val topGroup = findTopGroup(offenses)

    println(s"\nTop female-offender age-group with the highest number of offenses:\n Ages=${topGroup._1} offenses=${topGroup._2}")

    topGroup === expected
  }

  "Solution" should "find the average time the offended persons wait in the police station" in {
    val offenses = OffenseParser.parse(rawEvents)
    val expected = LocalTime.parse("05:23:24")
    val meanWaitTime = getMeanTime (offenses)

    println(s"\nMean time an offended person stays at the police station:\n ${meanWaitTime}\n")

    meanWaitTime === expected
  }

}
