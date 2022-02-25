package countdown.latch

import scala.concurrent.blocking
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.CountDownLatch

object Main extends App {
  val threadCount = 4
  val iterations  = 1000000
  var results     = Map.empty[Int, Int]

  for (_ <- 0 until iterations) {
    val consumersStarted  = new CountDownLatch(threadCount)
    val consumersFinished = new CountDownLatch(threadCount)
    val mainThreadProceed = new CountDownLatch(1)

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
