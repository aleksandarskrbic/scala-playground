package shapeless.guide.derivation

import shapeless._

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {
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

case class Employee(name: String, number: Int, manager: Boolean)
case class IceCream(name: String, numCherries: Int, inCone: Boolean)

object Demo extends App {
  import CsvEncoder._

  val a = CsvEncoder[IceCream]
  val b = implicitly[CsvEncoder[IceCream]]
  val c = the[CsvEncoder[IceCream]]
}
