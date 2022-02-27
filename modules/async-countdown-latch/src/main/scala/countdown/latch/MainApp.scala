package countdown.latch

import scala.concurrent.{ blocking, Await, Future, TimeoutException }
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.{ CountDownLatch => JCountDownLatch }
import scala.concurrent.duration.{ Duration, DurationInt }

object DefaultCountDownLatchDemo extends App {
  val threadCount = 4
  val iterations  = 10000
  var results     = Map.empty[Int, Int]

  for (_ <- 0 until iterations) {
    val consumersStarted  = new JCountDownLatch(threadCount)
    val consumersFinished = new JCountDownLatch(threadCount)
    val mainThreadProceed = new JCountDownLatch(1)

    var sharedState = 0

    for (_ <- 0 until threadCount) {
      global.execute(() =>
        blocking {
          consumersStarted.countDown()
          mainThreadProceed.await()
          sharedState += 1
          consumersFinished.countDown()
        }
      )
    }

    consumersStarted.await()
    mainThreadProceed.countDown()
    consumersFinished.await()

    results = results.updated(sharedState, results.getOrElse(sharedState, 0) + 1)
  }

  println("Finished ...")
  println(results)
}

object AsyncCountDownLatchDemo extends App {
  val threadCount = 10
  val iterations  = 100000
  var results     = Map.empty[Int, Int]

  for (_ <- 0 until iterations) {
    val consumersStarted  = new AsyncCountDownLatch(threadCount)
    val consumersFinished = new AsyncCountDownLatch(threadCount)
    val mainThreadProceed = new AsyncCountDownLatch(1)

    var sharedState = 0

    for (_ <- 0 until threadCount) {
      for {
        _ <- Future.unit
        _ = consumersStarted.countDown()
        _ <- mainThreadProceed.await(Duration.Inf)
      } yield {
        sharedState += 1
        consumersFinished.countDown()
      }
    }

    val await = for {
      _ <- consumersStarted.await(Duration.Inf)
      _ = mainThreadProceed.countDown()
      _ <- consumersFinished.await(1.millis)
    } yield {
      results = results.updated(sharedState, results.getOrElse(sharedState, 0) + 1)
    }

    try {
      Await.result(await, Duration.Inf)
    } catch {
      case _: TimeoutException =>
        results = results.updated(0, results.getOrElse(0, 0) + 1)
    }
  }

  println("Finished ...")
  println(results)
}
