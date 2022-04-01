package zio.examples

import zio.ZIO

final case class Person(firstname: String, lastname: String)

sealed trait ZIOApp {
  def run: ZIO[Any]

  def main(args: Array[String]): Unit = {
    run.run(result => println(s"Result was $result"))
    Thread.sleep(3500)
  }
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

  override def run: ZIO[Any] = ZIO.succeed(zipped)
}

object Map extends ZIOApp {
  val mapped: ZIO[Any] = ZIO.succeed(Person("John", "Doe")).map(_.lastname)

  override def run = mapped
}

object FlatMap extends ZIOApp {
  val mapped: ZIO[Any] = ZIO.succeed(Person("John", "Doe")).map(_.lastname)

  val flatMapped: ZIO[Any] = mapped.flatMap(res => ZIO.succeed(println(s"$res")).map(_ => "Done"))

  override def run: ZIO[Any] = flatMapped
}

object Async extends ZIOApp {
  override def run: ZIO[Any] =
    ZIO.async[Any] { complete =>
      println("Async started")
      Thread.sleep(1000)
      complete(10)
    }
}

object Fork extends ZIOApp {

  val asyncZIO = ZIO.async[Int] { complete =>
    println("Async started")
    Thread.sleep(2500)
    complete(scala.util.Random.nextInt(999))
  }

  def printLine(message: String): ZIO[Unit] =
    ZIO.succeed(println(message))

  val forkedZIO =
    for {
      fa <- asyncZIO.fork
      fb <- asyncZIO.fork
      _  <- printLine("fibers are forked!!!")
      a  <- fa.join
      b  <- fb.join
    } yield s"Results is $a and $b"

  override def run = forkedZIO
}

object ZipPar extends ZIOApp {
  val asyncZIO = ZIO.async[Int] { complete =>
    println("Async started")
    Thread.sleep(2500)
    complete(scala.util.Random.nextInt(999))
  }

  override def run = asyncZIO zipPar asyncZIO
}

object StackSafety extends ZIOApp {
  val program = ZIO.succeed(println("howdy")).repeat(10000)

  override def run = program
}
