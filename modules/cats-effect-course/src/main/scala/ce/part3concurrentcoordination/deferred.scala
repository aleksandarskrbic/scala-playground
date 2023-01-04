package ce.part3concurrentcoordination

import ce.util._
import cats.effect._
import cats.implicits._

import scala.concurrent.duration._

object deferred {}

object deferredTests extends IOApp.Simple {
  import deferred._

  override def run: IO[Unit] = ???
}
