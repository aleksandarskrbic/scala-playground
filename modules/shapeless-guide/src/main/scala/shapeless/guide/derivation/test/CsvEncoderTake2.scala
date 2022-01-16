package shapeless.guide.derivation.test

import shapeless._
import shapeless.guide.derivation.{ Employee, IceCream }

object CsvEncoderTake2 {
  // "Summoner"/"Materializer”, allows us to summon a type class instance given a target type
  def apply[A](implicit encoder: CsvEncoderToy[A]): CsvEncoderToy[A] =
    encoder

  // "Constructor" method
  def instance[A](fn: A => List[String]): CsvEncoderToy[A] =
    new CsvEncoderToy[A] {
      override def encode(value: A): List[String] =
        fn(value)
    }

  implicit val employeeEncoder: CsvEncoderToy[Employee] =
    instance { employee =>
      List(
        employee.name,
        employee.number.toString,
        if (employee.manager) "yes" else "no"
      )
    }

  implicit val iceCreamEncoder: CsvEncoderToy[IceCream] =
    instance { iceCream =>
      List(
        iceCream.name,
        iceCream.numCherries.toString,
        if (iceCream.inCone) "yes" else "no"
      )
    }

  implicit def pairEncoder[A, B](
    implicit
    aEncoder: CsvEncoderToy[A],
    bEncoder: CsvEncoderToy[B]
  ): CsvEncoderToy[(A, B)] =
    instance { pair: (A, B) =>
      val (a, b) = pair
      aEncoder.encode(a) ++ bEncoder.encode(b)
    }

  implicit val booleanEncoder: CsvEncoderToy[Boolean] =
    instance(b => if (b) List("yes") else List("no"))
}

object Demo2 extends App {
  import CsvEncoderTake2._

  val a = CsvEncoderTake2.apply[IceCream]
  val b = implicitly[CsvEncoderToy[IceCream]]
  val c = the[CsvEncoderToy[IceCream]]
}
