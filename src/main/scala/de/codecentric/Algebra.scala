package de.codecentric

import cats.arrow._
import cats._
import cats.`~>`
import cats.data.State
import cats.free.{ Free, FreeApplicative }
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

  def purely[A](p: TimerM[A]): A = {
    type S = (Option[LocalDateTime],Option[LocalDateTime])

    def currentStart: State[S,Option[LocalDateTime]] = State.inspect(_._1)
    def currentStop: State[S,Option[LocalDateTime]] = State.inspect(_._2)

    p.foldMap[State[S,?]](new (TimerF ~> State[S,?]) {
      def apply[B](fa: TimerF[B]): State[S,B] = fa match {
        case StartTimer =>
          Monad[State[S,?]].ifM(currentStart.map(_.isEmpty))(
            State.modify[S] { case (start,stop) => (Some(LocalDateTime.now),stop)},
            State.pure(()))

        case StopTimer => for {
          _ <- State.modify[S] { startStop => (startStop._1,Some(LocalDateTime.now)) }
          r <- Applicative[State[S,?]].compose[Option].map2(currentStart,currentStop)(TimerEntry(_,_))
          _ <- State.set((None: Option[LocalDateTime],None: Option[LocalDateTime]))
        } yield r

        case GetCurrentTimer =>
          Functor[State[S,?]].compose[Option].map(currentStart)(RunningTimerEntry(_))
          
        case DoWork(work) =>
          work.value
          State.pure(())
      }
    }).runA(None,None).value
  }
}

object TimerDsl extends TimerDsl
