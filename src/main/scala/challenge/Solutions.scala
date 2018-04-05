package challenge

import java.time.LocalTime
import scala.collection.immutable.SortedSet;

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
    val ageGroupOffensesOrder = Ordering.by[AgeGroupOffenses, Int](_._2)(Ordering.Int.reverse)

    val offensesByAge: Map[Int, Int] = offenses
      // take into account only offenses made by women
      .filter(_.offenderSex == "WOMAN")
      // group by the age of the offender
      .groupBy(_.offenderAge)
      // create map of Map[](offenderAge, numberOfOffenses) where numberOfOffenses is the total number of offenses per age
      .map{case (age, offensesList) => (age, offensesList.size)}

    // Compute the min and max ages to build 5 years groups
    // e.g. if the min age in the events is 23 and the max age is 54
    // minAge=20 and maxAge=55
    val maxOffensesAge = offensesByAge.keySet.max
    val minOffensesAge = offensesByAge.keySet.min
    val maxAge = maxOffensesAge + 5 - ((maxOffensesAge + 5) % 5)
    val minAge = minOffensesAge - (minOffensesAge % 5)

    val sortedGroups: SortedSet[AgeGroupOffenses] = (minAge to maxAge) // List ages within minAge and maxAge
      // groups ages with 5 elements.
      // i.e. creates age groups List(List(minAge, minAge+1, minAge+2, minAge+3, minAge+4), ..., List(maxAge-4, maxAge-3, maxAge-2, maxAge-1, maxAge))
      .grouped(5)
      // compute the number of offenses per age group
      .foldLeft(SortedSet[AgeGroupOffenses]()(ageGroupOffensesOrder))((orderedSet, agesInGroup) => {
        val totalOffenses = agesInGroup.foldLeft(0)((sum, age) => sum + offensesByAge.getOrElse(age, 0)) 
        orderedSet + (((agesInGroup.head, agesInGroup.last), totalOffenses))
      })
    
    sortedGroups.head
  }
}
