package challenge

import Solutions._

object Challenge {
  def main(args: Array[String]): Unit = {
    val offenses = OffenseParser.parse(args(0))

    //question 1
    val top10 = getTop10(offenses)

    //question 2 There's only one woman offenderwoman in the dataset, but if there where more I would use something like
    val topGroup = findTopGroup(offenses)

    // question 3
    val meanWaitTime = getMeanTime(offenses)

    println(top10)
    println(topGroup)
    println(meanWaitTime)
  }
}
