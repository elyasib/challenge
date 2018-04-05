package challenge

import java.time.LocalTime

object Solutions {
  type AgeGroup = (Int, Int)
  type AgeGroupOffenses = (AgeGroup, Int)

  // Computes the mean time an offended person stays at the police station
  def getMeanTime(offenses: Seq[Offense]) = {

    val times = offenses
      // compute the difference between reportEndTime and reportStartTime
      .map(offense => offense.reportEndTime.minusHours(offense.reportStartTime.getHour).minusMinutes(offense.reportStartTime.getMinute))
      // transform to relative seconds
      .map(_.toSecondOfDay)

    // compute the mean of the times
    LocalTime.ofSecondOfDay(times.sum / times.size)
  }

  // finds the top10 stations most dangerous to woman
  def getTop10(offenses: Seq[Offense]) = {
    val order = Ordering.by[(String, Int), Int](_._2)(Ordering.Int.reverse)

    offenses
      // group by subway station
      .groupBy(_.station)
      // creates a pair of (station, numberOfOffenses)
      .map{case (station, offensesList) => (station, offensesList.size)}
      .toList
      // sorts the list in descending number of offenses
      .sorted(order)
      // takes the top 10
      .take(10)
  }

  // Finds the female-offender age-group with the highest number of offenses.
  def findTopGroup(offenses: Seq[Offense]): AgeGroupOffenses = {
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
