package ce.part2concurrency

import ce.util._
import cats.effect._
import cats.implicits._
import cats.effect.kernel.Outcome.{ Canceled, Errored, Succeeded }

import scala.concurrent.duration._

object blockingIO {}

object blockingIOTest extends IOApp.Simple {
  import blockingIO._

  override def run: IO[Unit] = ???
}
