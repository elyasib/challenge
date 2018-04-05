package challenge

import java.time.LocalTime

object Solutions {

  def getMeanTime(offenses: Seq[Offense]) = {
    val times = offenses.map(offense => offense.reportEndTime.minusHours(offense.reportStartTime.getHour).minusMinutes(offense.reportStartTime.getMinute))
      .map(_.toSecondOfDay)

    LocalTime.ofSecondOfDay(times.sum / times.size)
  }

  def getTop10(offenses: Seq[Offense]) = {
    val order = Ordering.by[(String, Int), Int](_._2)(Ordering.Int.reverse)

    offenses.groupBy(_.station)
      .map{case (station, offensesList) => (station, offensesList.size)}
      .toList
      .sorted(order)
      .take(10)
  }

  def findTopGroup(offenses: Seq[Offense]) = {
    val topGroup = offenses.filter(_.offenderSex == "WOMAN")
      .groupBy(_.offenderAge)
      .map{case (age, offensesList) => (age, offensesList.size)}
      .toList
      .grouped(5)
      .map(group => {
        val offensesSum: Int = group.foldLeft(0)((acc, pair) => acc + pair._2)
        val startAge = group.head._1
        val endAge = group.last._1
        ((startAge, endAge), offensesSum)
      })
      .toList
      .head
    topGroup
  }
}
