package de.codecentric

import java.time.{ Duration, LocalDateTime }

case class TimerEntry(start: LocalDateTime, stop: LocalDateTime, id: Int) {
  def duration: Duration = Duration.between(start,stop)
}
