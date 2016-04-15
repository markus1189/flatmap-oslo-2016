package de.codecentric

import java.time.{ Duration, LocalDateTime }
import org.scalatest._

class TimerDslSpec extends FunSpec with Matchers {
  describe("TimerDsl") {
    describe("stopping the current timer") {
      it("should return None if no timer is running") {
        import TimerDsl.monadic._

        val timer: Option[TimerEntry] = TimerDsl.locally(stopTimer)
        timer should be(None)
      }
    }
  }
}
