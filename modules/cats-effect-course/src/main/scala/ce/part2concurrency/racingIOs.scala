package ce.part2concurrency

import ce.util._
import cats.effect._
import cats.implicits._
import cats.effect.kernel.Outcome.{ Canceled, Errored, Succeeded }

import scala.concurrent.duration._

object racingIOs {

  /**
   * Exercises:
   * 1 - implement a timeout pattern with race
   * 2 - a method to return a LOSING effect from a race (hint: use racePair)
   * 3 - implement race in terms of racePair
   */
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] =
    IO.race(io, IO.sleep(duration)).flatMap {
      case Left(v)  => IO(v)
      case Right(_) => IO.raiseError(new RuntimeException("Computation timed out."))
    }

  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((_, fibB)) => // fibB is loser fiber
        fibB.join.flatMap {
          case Succeeded(fb) => fb.map(_.asRight)
          case Errored(e)    => IO.raiseError(e)
          case Canceled()    => IO.raiseError(new RuntimeException("Canceled"))
        }
      case Right((fibA, _)) => // fibA is loser fiber
        fibA.join.flatMap {
          case Succeeded(fa) => fa.map(_.asLeft)
          case Errored(e)    => IO.raiseError(e)
          case Canceled()    => IO.raiseError(new RuntimeException("Canceled"))
        }
    }

  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((outA, fibB)) =>
        outA match {
          case Succeeded(effectA) => fibB.cancel >> effectA.map(a => Left(a))
          case Errored(e)         => fibB.cancel >> IO.raiseError(e)
          case Canceled() =>
            fibB.join.flatMap {
              case Succeeded(effectB) => effectB.map(b => Right(b))
              case Errored(e)         => IO.raiseError(e)
              case Canceled()         => IO.raiseError(new RuntimeException("Both computations canceled."))
            }
        }
      case Right((fibA, outB)) =>
        outB match {
          case Succeeded(effectB) => fibA.cancel >> effectB.map(b => Right(b))
          case Errored(e)         => fibA.cancel >> IO.raiseError(e)
          case Canceled() =>
            fibA.join.flatMap {
              case Succeeded(effectA) => effectA.map(a => Left(a))
              case Errored(e)         => IO.raiseError(e)
              case Canceled()         => IO.raiseError(new RuntimeException("Both computations canceled."))
            }
        }
    }
}

object racingIOsTest extends IOApp.Simple {
  import racingIOs._

  override def run: IO[Unit] =
    ???
}
