package de.codecentric

import java.time.{ Duration, LocalDateTime }

case class TimerEntry(start: LocalDateTime, stop: LocalDateTime) {
  def duration: Duration = Duration.between(start,stop)
}

case class RunningTimerEntry(start: LocalDateTime) extends AnyVal
