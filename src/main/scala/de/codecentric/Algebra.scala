package de.codecentric

import cats.free.Free
import cats.~>
import scala.concurrent.Future
import cats.std.future._
import scala.concurrent.ExecutionContext.Implicits.global
import java.time.LocalDate

object Timer {
  case class TimerEntry(day: LocalDate)

  sealed trait TimerF[A]
  case class GetTimerEntries(day: LocalDate) extends TimerF[Vector[TimerEntry]]

  type TimerM[A] = Free[TimerF, A]

  def getTimerEntries(day: LocalDate): TimerM[Vector[TimerEntry]] = Free.liftF(GetTimerEntries(day))

  def program: TimerM[Vector[TimerEntry]] = for {
    day1 <- getTimerEntries(LocalDate.now)
    day2 <- getTimerEntries(LocalDate.now)
    day3 <- getTimerEntries(LocalDate.now)
  } yield (day1 ++ day2 ++ day3)

  def step: TimerF ~> Future = new (TimerF ~> Future) {
    def apply[A](fa: TimerF[A]): Future[A] = fa match {
      case GetTimerEntries(day) => Future.successful(Vector())
    }
  }

  def main(args: Array[String]): Unit = {
    println(program.foldMap(step))
  }
}
