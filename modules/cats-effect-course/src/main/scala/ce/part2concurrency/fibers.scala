package ce.part2concurrency

import ce.util._
import cats.effect._
import scala.concurrent.duration._

object fibers extends IOApp.Simple {

  def createFiber: Fiber[IO, Throwable, String] = ???

  val fib: IO[Fiber[IO, Throwable, Int]] = IO.pure(42).debug.start

  val sameThreadIO: IO[Unit] =
    for {
      _ <- IO.pure(42).debug
      _ <- IO.pure("Scala").debug
    } yield ()

  val diffThreadIO: IO[Unit] =
    for {
      _ <- IO.pure(42).debug.start
      _ <- IO.pure("Java").debug
    } yield ()

  def cancelExample: IO[Outcome[IO, Throwable, String]] = {
    val task = IO("starting").debug >> IO.sleep(1.second) >> IO("done").debug

    val cancellationHandler = task.onCancel(IO("task is canceled").debug.void)

    for {
      fib    <- cancellationHandler.start
      _      <- IO.sleep(100.milliseconds) >> IO("canceling").debug
      _      <- fib.cancel
      result <- fib.join
    } yield result
  }

  override def run: IO[Unit] =
    cancelExample.debug.void
}

/**
 * Exercises:
 *  1. Write a function that runs an IO on another thread, and, depending on the result of the fiber
 *    - return the result in an IO
 *    - if errored or cancelled, return a failed IO
 *
 * 2. Write a function that takes two IOs, runs them on different fibers and returns an IO with a tuple containing both results.
 *    - if both IOs complete successfully, tuple their results
 *    - if the first IO returns an error, raise that error (ignoring the second IO's result/error)
 *    - if the first IO doesn't error but second IO returns an error, raise that error
 *    - if one (or both) canceled, raise a RuntimeException
 *
 * 3. Write a function that adds a timeout to an IO:
 *    - IO runs on a fiber
 *    - if the timeout duration passes, then the fiber is canceled
 *    - the method returns an IO[A] which contains
 *      - the original value if the computation is successful before the timeout signal
 *      - the exception if the computation is failed before the timeout signal
 *      - a RuntimeException if it times out (i.e. cancelled by the timeout)
 */
object exercises {
  def processResultsFromFiber[A](io: IO[A]): IO[A] =
    io.start.debug.flatMap { fib =>
      fib.join.flatMap {
        case Outcome.Succeeded(fa)  => fa
        case Outcome.Errored(error) => IO.raiseError(error)
        case Outcome.Canceled()     => IO.raiseError(new RuntimeException(s"Fiber cancelled"))
      }
    }

  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] =
    for {
      fibA <- ioa.start
      fibB <- iob.start
      a    <- fibA.join
      b    <- fibB.join
      result <- (a, b) match {
                 case (Outcome.Succeeded(fa), Outcome.Succeeded(fb)) =>
                   for {
                     a <- fa
                     b <- fb
                   } yield a -> b
                 case (Outcome.Errored(error), _) =>
                   IO.raiseError(error)
                 case (_, Outcome.Errored(error)) =>
                   IO.raiseError(error)
                 case _ =>
                   IO.raiseError(new RuntimeException(s"Some fiber cancelled"))
               }
    } yield result

  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] =
    for {
      fib     <- io.start
      _       <- (IO.sleep(duration) >> fib.cancel).start
      outcome <- fib.join
      result <- outcome match {
                 case Outcome.Succeeded(fa)  => fa
                 case Outcome.Errored(error) => IO.raiseError(error)
                 case Outcome.Canceled()     => IO.raiseError(new RuntimeException(s"Fiber cancelled"))
               }
    } yield result
}

object fibersTest extends IOApp.Simple {
  import exercises._

  def testEx1() = {
    val aComputation = IO("starting").debug >> IO.sleep(1.second) >> IO("done!").debug >> IO(42)
    processResultsFromFiber(aComputation).void
  }

  def testEx2() = {
    val firstIO  = IO.sleep(2.seconds) >> IO(1).debug
    val secondIO = IO.sleep(3.seconds) >> IO(2).debug
    tupleIOs(firstIO, secondIO).debug.void
  }

  def testEx3() = {
    val aComputation = IO("starting").debug >> IO.sleep(1.second) >> IO("done!").debug >> IO(42)
    timeout(aComputation, 500.millis).debug.void
  }

  override def run: IO[Unit] =
    testEx3
}
