package ce.part3concurrentcoordination

import ce.util._
import cats.effect._
import cats.implicits._

import scala.concurrent.duration._

object refs {
  val atomicV   = Ref[IO].of(100)
  val increased = atomicV.flatMap(ref => ref.set(101))

  def demoConcurrentWorkPure(): IO[Unit] = {
    def task(workload: String, total: Ref[IO, Int]): IO[Unit] = {
      val wordCount = workload.split(" ").length

      for {
        _        <- IO(s"Counting words for '$workload': $wordCount'").debug
        newCount <- total.updateAndGet(currentCount => currentCount + wordCount)
        _        <- IO(s"New total: $newCount").debug
      } yield ()
    }

    for {
      initialCount <- Ref[IO].of(0)
      _ <- List("I love Cats Effect", "This ref thing is useless", "Daniel writes a lot of code")
            .map(string => task(string, initialCount))
            .parSequence
    } yield ()
  }

  /**
   * Exercise
   */
  def tickingClockImpure(): IO[Unit] = {
    var ticks: Long = 0L

    def tickingClock: IO[Unit] =
      for {
        _ <- IO.sleep(1.second)
        _ <- IO(System.currentTimeMillis()).debug
        _ <- IO(ticks += 1) // not thread safe
        _ <- tickingClock
      } yield ()

    def printTicks: IO[Unit] =
      for {
        _ <- IO.sleep(5.seconds)
        _ <- IO(s"TICKS: $ticks").debug
        _ <- printTicks
      } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }

  def tickingClockPure(): IO[Unit] =
    Ref[IO].of(0L).flatMap { ref =>
      def tickingClock(ref: Ref[IO, Long]): IO[Unit] =
        for {
          _ <- IO.sleep(1.second)
          _ <- IO(System.currentTimeMillis()).debug
          _ <- ref.update(_ + 1)
          _ <- tickingClock(ref)
        } yield ()

      def printTicks(ref: Ref[IO, Long]): IO[Unit] =
        for {
          _       <- IO.sleep(5.seconds)
          message <- ref.modify(ticks => (ticks, s"TICKS: $ticks"))
          _       <- IO(message).debug
          _       <- printTicks(ref)
        } yield ()

      for {
        _ <- (tickingClock(ref), printTicks(ref)).parTupled
      } yield ()
    }
}

object refsTests extends IOApp.Simple {
  import refs._

  override def run: IO[Unit] = tickingClockPure
}
