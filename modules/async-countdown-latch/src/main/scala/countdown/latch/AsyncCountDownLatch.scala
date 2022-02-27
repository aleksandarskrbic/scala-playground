package countdown.latch

import scala.annotation.tailrec
import scala.concurrent.duration.{ Duration, FiniteDuration }
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ Executors, RejectedExecutionException, ScheduledExecutorService }
import scala.concurrent.{ ExecutionContext, Future, Promise, TimeoutException }

final class AsyncCountDownLatch(count: Int)(implicit ec: ExecutionContext) extends AutoCloseable {
  import AsyncCountDownLatch.State

  private[this] val state: AtomicReference[State] = new AtomicReference(State(count, Set.empty))

  private[this] val scheduler: ScheduledExecutorService =
    Executors.newSingleThreadScheduledExecutor { runnable =>
      val thread = new Thread(runnable)
      thread.setName(s"countdownlatch-${hashCode()}")
      thread.setDaemon(true)
      thread
    }

  private def installTimeout(p: Promise[Unit], timeout: Duration): Unit = {
    @tailrec def removePromise(): Unit =
      state.get() match {
        case current @ State(count, tasks) =>
          val update = State(count, tasks - p)
          if (!state.compareAndSet(current, update))
            removePromise()
        case null =>
          ()
      }

    val ex = new TimeoutException(s"AsyncCountDownLatch.await($timeout)")

    val timeoutTask: Runnable = () => {
      removePromise()
      p.tryFailure(ex)
    }

    try {
      val cancelToken = scheduler.schedule(timeoutTask, timeout.length, timeout.unit)

      p.future.onComplete { res =>
        val trigger = res.isSuccess || res.failed.get != ex
        if (trigger) cancelToken.cancel(true)
      }
    } catch {
      case ex: RejectedExecutionException =>
        ()
    }
  }

  @tailrec def countDown(): Unit =
    state.get() match {
      case current @ State(count, tasks) =>
        if (count > 0) {
          val update = State(count - 1, tasks)
          if (!state.compareAndSet(current, update))
            countDown() // retry until succeed
          else if (update.count == 0) {
            for (t <- tasks) t.trySuccess(()) // trigger tasks
            close() // state.lazySet(null)  GC purposes
          }
        }
      case null =>
        ()
    }

  @tailrec def await(timeout: Duration): Future[Unit] =
    state.get() match {
      case current @ State(count, tasks) =>
        if (count == 0)
          Future.unit
        else {
          val p      = Promise[Unit]()
          val update = State(count, tasks + p)

          timeout match {
            case d: FiniteDuration =>
              installTimeout(p, d)
            case _ =>
              ()
          }

          if (!state.compareAndSet(current, update))
            await(timeout) // retry until succeed
          else
            p.future
        }
      case null =>
        Future.unit
    }

  override def close(): Unit = {
    state.lazySet(null)
    scheduler.shutdown()
  }
}

object AsyncCountDownLatch {
  private final case class State(count: Int, task: Set[Promise[Unit]])
}
