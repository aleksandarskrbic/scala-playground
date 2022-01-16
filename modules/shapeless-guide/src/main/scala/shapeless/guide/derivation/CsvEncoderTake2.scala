package shapeless.guide.derivation

import shapeless._

object CsvEncoderTake2 {
  // "Summoner"/"Materializer”, allows us to summon a type class instance given a target type
  def apply[A](implicit encoder: CsvEncoder[A]): CsvEncoder[A] =
    encoder

  // "Constructor" method
  def instance[A](fn: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
      override def encode(value: A): List[String] =
        fn(value)
    }

  implicit val employeeEncoder: CsvEncoder[Employee] =
    instance { employee =>
      List(
        employee.name,
        employee.number.toString,
        if (employee.manager) "yes" else "no"
      )
    }

  implicit val iceCreamEncoder: CsvEncoder[IceCream] =
    instance { iceCream =>
      List(
        iceCream.name,
        iceCream.numCherries.toString,
        if (iceCream.inCone) "yes" else "no"
      )
    }

  implicit def pairEncoder[A, B](
    implicit
    aEncoder: CsvEncoder[A],
    bEncoder: CsvEncoder[B]
  ): CsvEncoder[(A, B)] =
    instance { pair: (A, B) =>
      val (a, b) = pair
      aEncoder.encode(a) ++ bEncoder.encode(b)
    }

  implicit val booleanEncoder: CsvEncoder[Boolean] =
    instance(b => if (b) List("yes") else List("no"))
}

object Demo2 extends App {
  import CsvEncoderTake2._

  val a = CsvEncoderTake2.apply[IceCream]
  val b = implicitly[CsvEncoder[IceCream]]
  val c = the[CsvEncoder[IceCream]]
}
