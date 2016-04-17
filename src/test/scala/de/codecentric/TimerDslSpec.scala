package de.codecentric

import cats.data.State
import cats.std.list._
import cats.syntax.traverse._
import cats.{ Applicative, Monad }
import java.time.{ Duration, LocalDateTime }
import org.scalatest._
import cats.syntax.cartesian._

class TimerDslSpec extends FunSpec with Matchers with Inspectors {
  describe("Monadic TimerDsl") {
    import TimerDsl._
    import TimerDsl.monadic._

    describe("stopping the current timer") {
      it("should return None if no timer is running") {
        val timer: Option[TimerEntry] = TimerDsl.purely(stopTimer)

        timer should be(None)
      }

      it("should return the finished timer") {
        val timer: Option[TimerEntry] = TimerDsl.purely {
          for {
            _ <- startTimer
            t <- stopTimer
          } yield t
        }

        timer shouldBe defined
      }

      it("should return the finished timer repeatedly") {
        val n: Int = 3

        def startStop[A]: TimerM[Option[TimerEntry]] = for {
          _ <- startTimer
          _ <- doWork(Thread.sleep(10))
          t <- stopTimer
        } yield t

        val entries: TimerM[List[Option[TimerEntry]]] =
          List.fill(n)(startStop).sequence

        val results = TimerDsl.purely(entries)

        val starts = results.flatten.map(_.start)
        val stops = results.flatten.map(_.stop)

        results.flatten should have length(n.toLong)
        starts.distinct should have length(n.toLong)
        stops.distinct should have length(n.toLong)
      }
    }
  }

  describe("Applicative TimerDSL") {
    import TimerDsl._
    import TimerDsl.applicative._

    it("should work with a simple start/doWork/stop program") {
      type T[A] = State[(Option[LocalDateTime],Option[LocalDateTime]),A]

      val program: TimerA[Option[TimerEntry]] =
        startTimer *> doWork(Thread.sleep(10)) *> stopTimer

      val result: Option[TimerEntry] =
        program.foldMap[T](TimerDsl.interpTimerF).runA((None,None)).value

      result shouldBe defined
    }
  }
}
