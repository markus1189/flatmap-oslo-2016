package de.codecentric

import cats.free.{ Free, FreeApplicative }
import java.time.LocalDate

object Algebra {
  sealed trait TimerF[A]
  type TimerM[A] = Free[TimerF, A]
  type TimerA[A] = FreeApplicative[TimerF, A]

  trait Commands {
    case object StartTimer extends TimerF[Unit]
    case object StopTimer extends TimerF[Unit]
    case object GetCurrentTimer extends TimerF[Option[TimerEntry]]
  }

  trait Reporting {
    case class GetTimerEntries(day: LocalDate) extends TimerF[Vector[TimerEntry]]
    def getTimerEntries(day: LocalDate): TimerM[Vector[TimerEntry]] = Free.liftF(GetTimerEntries(day))
  }
}
