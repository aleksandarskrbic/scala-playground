package essential.effects.part1

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object Printing extends App {
  lazy val hello = Effect.putStr("hello!")
  lazy val world = Effect.putStr("world!")

  val helloWorld =
    for {
      _ <- hello
      _ <- world
    } yield ()

  helloWorld.unsafeRun()
}

object Timing extends App {

  val clock: Effect[Long] =
    Effect(() => System.currentTimeMillis)

  def time[A](action: Effect[A]): Effect[(FiniteDuration, A)] =
    for {
      start <- clock
      a     <- action
      end   <- clock
    } yield (FiniteDuration(end - start, TimeUnit.MILLISECONDS), a)

  val timedHello = Timing.time(Printing.hello)

  timedHello.unsafeRun() match {
    case (duration, _) => println(s"'hello' took $duration")
  }
}
