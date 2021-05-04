package essential.effects.part2

import cats.effect._
import cats.effect.kernel.Resource.ExitCase
import cats.effect.kernel.Resource.ExitCase._
import essential.effects.debug._

import scala.concurrent.duration._

object TickingClock extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    tickingClock.guaranteeCase { outcome =>
      fromOutcome(outcome) match {
        case ExitCase.Canceled   => IO("canceled").debug().void
        case ExitCase.Succeeded  => IO("completed").debug().void
        case ExitCase.Errored(t) => IO(s"error: $t").debug().void
      }
    }.as(ExitCode.Success)

  val tickingClock: IO[Unit] =
    for {
      _ <- IO(System.currentTimeMillis).debug()
      _ <- IO.sleep(1.second)
      _ <- tickingClock
    } yield ()
}
