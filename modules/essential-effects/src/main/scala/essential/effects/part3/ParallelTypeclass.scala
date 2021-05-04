package essential.effects.part3

import cats.effect._
import cats.implicits._
import essential.effects.debug._

object ParMapN extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    par.as(ExitCode.Success)

  private val hello = IO("Hello").debug
  private val world = IO("World").debug

  val par: IO[String] = (hello, world).parMapN((h, w) => s"$h $w").debug
}

object ParMapNErrors extends IOApp {

  def run(args: List[String]): IO[ExitCode] =
    e1.attempt.debug *>
      IO("---").debug *>
      e2.attempt.debug *>
      IO("---").debug *>
      e3.attempt.debug *>
      IO.pure(ExitCode.Success)

  private val ok = IO("hi").debug
  private val err1 = IO.raiseError[String](new RuntimeException("Error 1!")).debug
  private val err2 = IO.raiseError[String](new RuntimeException("Error 2!")).debug

  private val e1 = (ok, err1).parMapN((_, _) => ())
  private val e2 = (err1, ok).parTupled.void
  private val e3 = (err1, err2).parTupled.void
}

object ParTraverse extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    tasks
      .parTraverse(task)
      .debug
      .as(ExitCode.Success)

  val numTasks = 100
  val tasks: List[Int] = List.range(0, numTasks)

  def task(id: Int): IO[Int] = IO(id).debug
}

