package ce.part2concurrency

import ce.util._
import cats.effect._
import cats.implicits._
import cats.effect.kernel.Outcome.{ Canceled, Errored, Succeeded }

import scala.concurrent.duration._

object resources {
  final case class Connection(url: String) {
    def open(): IO[String]  = IO(s"opening connection to $url").debug
    def close(): IO[String] = IO(s"closing connection to $url").debug
  }

  // problem: leaking resources
  def asyncFetchUrl: IO[Unit] =
    for {
      fib <- (Connection("www.google.com").open() *> IO.sleep((Int.MaxValue).seconds)).start
      _   <- IO.sleep(1.second) *> fib.cancel
    } yield ()

  def correctAsyncFetchUrl: IO[Unit] =
    for {
      conn <- IO(Connection("www.google.com"))
      fib  <- (conn.open() *> IO.sleep((Int.MaxValue).seconds)).onCancel(conn.close().void).start
      _    <- IO.sleep(1.second) *> fib.cancel
    } yield ()

  // bracket pattern
  def bracketFetchUrl: IO[Unit] =
    IO(Connection("www.google.com"))
      .bracket(conn => conn.open() *> IO.sleep((Int.MaxValue).seconds))(_.close().void)

  def bracketProgram =
    for {
      fib <- bracketFetchUrl.start
      _   <- IO.sleep(1.second) *> fib.cancel
    } yield ()

  // resources
  val connectionResource: Resource[IO, Connection] =
    Resource.make(IO(Connection("rockthejvm.com")))(conn => conn.close().void)

  def resourceFetchUrl: IO[Unit] =
    for {
      fib <- connectionResource.use(conn => conn.open() >> IO.never).start
      _   <- IO.sleep(1.second) >> fib.cancel
    } yield ()

  // finalizers to regular IOs
  val ioWithFinalizer = IO("some resource").debug.guarantee(IO("freeing resource").debug.void)
  val ioWithFinalizer_v2 = IO("some resource").debug.guaranteeCase {
    case Succeeded(fa) => fa.flatMap(result => IO(s"releasing resource: $result").debug).void
    case Errored(e)    => IO("nothing to release").debug.void
    case Canceled()    => IO("resource got canceled, releasing what's left").debug.void
  }
}

object resourcesTest extends IOApp.Simple {
  import resources._

  override def run: IO[Unit] =
    resourceFetchUrl
}
