package zio.examples

import zio.ZIO

final case class Person(firstname: String, lastname: String)

sealed trait ZIOApp {
  def run: ZIO[Any]

  def main(args: Array[String]): Unit =
    run.run(result => println(s"Result was $result"))
}

object SucceedNow extends ZIOApp {
  override def run: ZIO[Any] = ZIO.succeedNow(Person("John", "Doe"))
}

object SucceedNowPrint extends ZIOApp {
  val howdy: ZIO[Unit] = ZIO.succeed(println("Howdy"))

  override def run: ZIO[Any] = ZIO.succeedNow(1)
}

object Zip extends ZIOApp {
  val zipped: ZIO[(Int, String)] = ZIO.succeed(1) zip ZIO.succeed("one")

  override def run: ZIO[Any] = ZIO.succeed(zipped.steps)
}

object Map extends ZIOApp {
  val mapped: ZIO[String] = ZIO.succeed(Person("John", "Doe")).map(_.lastname)

  override def run: ZIO[Any] = mapped
}

object FlatMap extends ZIOApp {
  val mapped: ZIO[String] = ZIO.succeed(Person("John", "Doe")).map(_.lastname)

  val flatMapped = mapped.flatMap(res => ZIO.succeed(println(s"$res")).map(_ => "Done"))

  override def run: ZIO[Any] = flatMapped
}
