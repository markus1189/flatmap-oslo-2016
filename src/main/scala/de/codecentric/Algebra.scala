package de.codecentric

import cats.arrow._
import cats._
import cats.`~>`
import cats.data.State
import cats.data.Coproduct
import cats.free.{ Free, FreeApplicative }
import cats.std.option._
import java.time.{ LocalDate, LocalDateTime }

trait Types {
  type FreeMA[F[_],A] = Free[Coproduct[F,FreeApplicative[F, ?],?],A]
}

trait TimerDsl extends Serializable with Types {
  sealed trait TimerF[A]
  type TimerM[A] = Free[TimerF, A]
  type TimerA[A] = FreeApplicative[TimerF, A]
  type TimerMF[A] = Coproduct[TimerF,TimerA,A]
  type Timer[A] = FreeMA[TimerF,A]

  private case object StartTimer extends TimerF[Unit]
  private case object StopTimer extends TimerF[Option[TimerEntry]]
  private case object GetCurrentTimer extends TimerF[Option[RunningTimerEntry]]
  private case class DoWork(work: Eval[Any]) extends TimerF[Unit]

  object both extends Serializable {
    def startTimer: Timer[Unit] =
      Free.liftF[TimerMF,Unit](Coproduct.left(StartTimer))

    def stopTimer: Timer[Option[TimerEntry]] =
      Free.liftF[TimerMF,Option[TimerEntry]](Coproduct.left(StopTimer))

    def getCurrentTimer: Timer[Option[RunningTimerEntry]] =
      Free.liftF[TimerMF,Option[RunningTimerEntry]](Coproduct.left(GetCurrentTimer))

    def doWork[A](work: => A): Timer[Unit] =
      Free.liftF[TimerMF,Unit](Coproduct.left(DoWork(Eval.later(work))))

    def embed[A](p: TimerA[A]) =
      Free.liftF[TimerMF,A](Coproduct.right(p))
  }

  object monadic extends Serializable {
    def startTimer: TimerM[Unit] = Free.liftF(StartTimer)
    def stopTimer: TimerM[Option[TimerEntry]] = Free.liftF(StopTimer)
    def getCurrentTimer: TimerM[Option[RunningTimerEntry]] = Free.liftF(GetCurrentTimer)
    def doWork[A](work: => A): TimerM[Unit] = Free.liftF(DoWork(Eval.later(work)))
  }

  object applicative extends Serializable {
    def startTimer: TimerA[Unit] = FreeApplicative.lift(StartTimer)
    def stopTimer: TimerA[Option[TimerEntry]] = FreeApplicative.lift(StopTimer)
    def getCurrentTimer: TimerA[Option[RunningTimerEntry]] = FreeApplicative.lift(GetCurrentTimer)
    def doWork[A](work: => A): TimerA[Unit] = FreeApplicative.lift(DoWork(Eval.later(work)))
  }

  val StateOption = Applicative[State[S,?]].compose[Option]
  type S = (Option[LocalDateTime],Option[LocalDateTime])
  def currentStart: State[S,Option[LocalDateTime]] = State.inspect(_._1)
  def currentStop: State[S,Option[LocalDateTime]] = State.inspect(_._2)

  val interpTimerF: TimerF ~> State[S,?] = new (TimerF ~> State[S,?]) {
    def apply[B](fa: TimerF[B]): State[S,B] = fa match {
      case StartTimer =>
        Monad[State[S,?]].ifM(currentStart.map(_.isEmpty))(
          State.modify[S] { case (start,stop) => (Some(LocalDateTime.now),stop)},
          State.pure(()))

      case StopTimer => for {
        _ <- State.modify[S] { startStop => (startStop._1,Some(LocalDateTime.now)) }
        r <- StateOption.map2(currentStart,currentStop)(TimerEntry(_,_))
        _ <- State.set((None: Option[LocalDateTime],None: Option[LocalDateTime]))
      } yield r

      case GetCurrentTimer =>
        StateOption.map(currentStart)(RunningTimerEntry(_))

      case DoWork(work) =>
        work.value
        State.pure(())
    }
  }

  val interpTimerA: TimerA ~> State[S,?] = new (TimerA ~> State[S,?]) {
    def apply[B](fa: TimerA[B]): State[S,B] = {
      fa.monad.foldMap[State[S,?]](interpTimerF)
    }
  }

  def purely[A](p: TimerM[A]): A = {
    p.foldMap[State[S,?]](interpTimerF).runA((None,None)).value
  }

  def runTimer[A](p: Timer[A]): A = {
    val compile: Coproduct[TimerF, TimerA, ?] ~> State[S,?] = interpTimerF.or[TimerA](interpTimerA)
    p.foldMap[State[S,?]](compile).runA((None,None)).value
  }
}

object TimerDsl extends TimerDsl
