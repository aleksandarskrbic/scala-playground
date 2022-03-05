package ce

import ce.util._
import cats.effect._

object fibers1 extends IOApp.Simple {

  val sameThreadIO: IO[Unit] =
    for {
      _ <- IO.pure(42).debug
      _ <- IO.pure("Scala").debug
    } yield ()

  def createFiber: Fiber[IO, Throwable, String] = ???

  val fib: IO[Fiber[IO, Throwable, Int]] = IO.pure(42).debug.start

  val diffThreadIO: IO[Unit] =
    for {
      _ <- fib
      _ <- IO.pure("Java").debug
    } yield ()

  override def run: IO[Unit] =
    diffThreadIO
}
