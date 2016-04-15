package de.codecentric

import cats.Applicative
import cats.free.{ Free, FreeApplicative }
import cats.`~>`
import cats._
import cats.std.option._
import java.time.{ LocalDate, LocalDateTime }

trait TimerDsl extends Serializable {
  sealed trait TimerF[A]
  type TimerM[A] = Free[TimerF, A]
  type TimerA[A] = FreeApplicative[TimerF, A]

  private case object StartTimer extends TimerF[Unit]
  private case object StopTimer extends TimerF[Option[TimerEntry]]
  private case object GetCurrentTimer extends TimerF[Option[RunningTimerEntry]]
  private case class DoWork(work: Eval[Any]) extends TimerF[Unit]

  // case class GetTimerEntries(day: LocalDate) extends TimerF[Vector[TimerEntry]]

  object monadic extends Serializable {
    def startTimer: TimerM[Unit] = Free.liftF(StartTimer)
    def stopTimer: TimerM[Option[TimerEntry]] = Free.liftF(StopTimer)
    def getCurrentTimer: TimerM[Option[RunningTimerEntry]] = Free.liftF(GetCurrentTimer)
    def doWork[A](work: => A): TimerM[Unit] = Free.liftF(DoWork(Eval.later(work)))
    // def getTimerEntries(day: LocalDate): TimerM[Vector[TimerEntry]] = Free.liftF(GetTimerEntries(day))
  }

  def locally[A](p: TimerM[A]): A = {
    var currentStart: Option[LocalDateTime] = None
    var currentStop: Option[LocalDateTime] = None

    p.foldMap(new (TimerF ~> Id) {
      def apply[B](fa: TimerF[B]): Id[B] = fa match {
        case StartTimer =>
          if (currentStart.isEmpty) currentStart = Some(LocalDateTime.now)
        case StopTimer =>
          currentStop = Some(LocalDateTime.now)
          val r = Applicative[Option].map2(currentStart, currentStop)(TimerEntry(_,_))
          currentStart = None
          currentStop = None
          r
        case GetCurrentTimer =>
          currentStart.map(RunningTimerEntry)
        case DoWork(work) =>
          work.value
          ()
      }
    })
  }
}

object TimerDsl extends TimerDsl
