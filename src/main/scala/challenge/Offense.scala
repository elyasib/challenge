package challenge

import java.time.LocalTime

case class Offense(station: String,
                   line: String,
                   day: String,
                   reportStartTime: LocalTime,
                   reportEndTime: LocalTime,
                   description: String,
                   offense: String,
                   victimSex: String,
                   victimAge: Int,
                   offenderSex: String,
                   offenderAge: Int,
                   offenderDetained: Boolean)

