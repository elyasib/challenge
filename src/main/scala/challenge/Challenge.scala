package challenge

import Solutions._

object Challenge {
  def main(args: Array[String]): Unit = {
    val offenses = OffenseParser.parse(args(0))

    //question 1
    val top10 = getTop10(offenses)

    //question 2 There's only one offender woman in the dataset, but if there were more I would use something like this:
    val topGroup = findTopGroup(offenses)

    // question 3
    val meanWaitTime = getMeanTime(offenses)

    // question 4 I sent my answer to this question through email but I'll copy my answer here so it doesn't get lost.
    // Answer:  I think there might be multiple reasons, amongst them,
    //   1. The events are from weekdays only.
    //   2. The event log is based on reported offenses but there might be several more offenses that go unreported. One valid,
    //      if extreme, argument one can make is that perhaps people that are offended in some of the stations with the least amount of
    //      reports never report the offense because they get killed, or they are offended by a police officer. (Similar to the problem of
    //      shot down planes during WW2 that Abraham Wald helped solving).
    //   3. The data sample could be too small, in terms of sampling time and absolute size.

    println(s"\nTop10 stations most dangerous for woman:\n${top10.mkString(",")}")

    println(s"\nTop female-offender age-group with the highest number of offenses:\n Ages=${topGroup._1} offenses=${topGroup._2}")

    println(s"\nMean time an offended person stays at the police station:\n ${meanWaitTime}\n")
  }
}
