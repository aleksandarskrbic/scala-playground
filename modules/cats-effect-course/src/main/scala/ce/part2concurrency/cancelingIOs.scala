package ce.part2concurrency

import ce.util._
import cats.effect._
import cats.implicits._
import cats.effect.kernel.Outcome.{ Canceled, Errored, Succeeded }

import scala.concurrent.duration._

object cancelingIOs {}

object cancelingIOsTest extends IOApp.Simple {
  import cancelingIOs._

  override def run: IO[Unit] =
    ???
}
