package countdown.latch

import scala.concurrent.{ Future, Promise }
import java.util.concurrent.atomic.AtomicReference

final class AsyncCountDownLatch(count: Int) {
  import AsyncCountDownLatch.State

  private[this] val state = new AtomicReference(State(count, Nil))

  def countDown(): Unit = ???

  def await(): Future[Unit] = ???
}

object AsyncCountDownLatch {
  private final case class State(counter: Int, task: List[Promise[Unit]])
}
