package essential.effects.part2

import cats.effect._

object HelloWorld extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    helloWorld.as(ExitCode.Success)

  val helloWorld: IO[Unit] =
    for {
      _ <- IO(println("Hello"))
      _ <- IO(println("World"))
    } yield ()
}
