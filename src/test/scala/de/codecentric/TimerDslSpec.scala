package de.codecentric

import cats.{ Applicative, Monad }
import java.time.{ Duration, LocalDateTime }
import org.scalatest._
import cats.std.list._

class TimerDslSpec extends FunSpec with Matchers with Inspectors {
  describe("TimerDsl") {
    import TimerDsl._
    import TimerDsl.monadic._

    describe("stopping the current timer") {
      it("should return None if no timer is running") {
        val timer: Option[TimerEntry] = TimerDsl.locally(stopTimer)

        timer should be(None)
      }

      it("should return the finished timer") {
        val timer: Option[TimerEntry] = TimerDsl.locally {
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
          _ = Thread.sleep(10)
          t <- stopTimer
        } yield t

        val entries: TimerM[List[Option[TimerEntry]]] =
          Applicative[TimerM].sequence(List.fill(n)(startStop))

        val results = TimerDsl.locally(entries)

        val starts = results.flatten.map(_.start)
        println(starts)
        val stops = results.flatten.map(_.stop)

        results.flatten should have length(n.toLong)
        starts.distinct should have length(n.toLong)
        stops.distinct should have length(n.toLong)
      }
    }
  }
}
