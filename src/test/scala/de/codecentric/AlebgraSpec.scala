package de.codecentric

import java.time.{ Duration, LocalDateTime }
import org.scalatest._

class TimerEntrySpec extends FlatSpec with Matchers {

  "TimerEntry" should "calculate the duration" in {
    val start = LocalDateTime.of(2016, 4, 15, 10, 0)
    val end = LocalDateTime.of(2016, 4, 15, 12, 0)

    TimerEntry(start,end,1).duration should be(Duration.ofHours(2))
  }
}
