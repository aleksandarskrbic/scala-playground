package ce.part2concurrency

import ce.util._
import cats.effect._
import cats.implicits._
import cats.effect.kernel.Outcome.{ Canceled, Errored, Succeeded }

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Try

object asyncIO {
  // IOs can run asynchronously on fibers, without having to manually manage the fiber lifecycle
  val threadPool                    = Executors.newFixedThreadPool(8)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(threadPool)
  type Callback[A] = Either[Throwable, A] => Unit

  def computeMeaningOfLife(): Int = {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computing the meaning of life on some other thread...")
    42
  }

  def computeMeaningOfLifeEither(): Either[Throwable, Int] =
    Try(computeMeaningOfLife()).toEither

  def computeMolOnThreadPool(): Unit =
    threadPool.execute(() => computeMeaningOfLife())

  // lift computation to an IO
  // async is a FFI
  val asyncMolIO: IO[Int] =
    IO.async_ { (cb: Callback[Int]) => // CE thread blocks (semantically) until this cb is invoked (by some other thread)
      threadPool.execute { () =>       // computation not managed by CE
        val result = computeMeaningOfLifeEither()
        cb(result) // CE thread is notified with the result
      }
    }
}

object asyncIOTest extends IOApp.Simple {
  import asyncIO._

  /**
   * Exercise: lift an async computation on ec to an IO.
   */
  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] =
    IO.async_[A] { cb =>
      ec.execute { () =>
        val result = Try(computation()).toEither
        cb(result)
      }
    }

  def demoAsyncCancellation() = {
    val asyncMeaningOfLifeIO_v2: IO[Int] = IO.async { (cb: Callback[Int]) =>
      /**
       * finalizer in case computation gets cancelled.
       * finalizers are of type IO[Unit]
       * not specifying finalizer => Option[IO[Unit]]
       * creating option is an effect => IO[Option[IO[Unit]]]
       * return IO[Option[IO[Unit]]]
       */
      IO {
        threadPool.execute { () =>
          val result = computeMeaningOfLifeEither()
          cb(result)
        }
      }.as(Some(IO("Cancelled!").debug.void))
    }

    for {
      fib <- asyncMeaningOfLifeIO_v2.start
      _   <- IO.sleep(500.millis) >> IO("cancelling...").debug >> fib.cancel
      _   <- fib.join
    } yield ()
  }

  override def run: IO[Unit] =
    demoAsyncCancellation
}
