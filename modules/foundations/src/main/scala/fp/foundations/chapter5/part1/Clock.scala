package fp.foundations.chapter5.part1

import java.time.Instant

trait Clock {
  def now(): Instant
}

object Clock {
  val system: Clock = new Clock {
    def now(): Instant =
      Instant.now()
  }

  def constant(instant: Instant): Clock = new Clock {
    def now(): Instant = instant
  }
}
