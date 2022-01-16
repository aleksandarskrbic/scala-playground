package shapeless.guide.derivation

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

case class Employee(name: String, number: Int, manager: Boolean)
case class IceCream(name: String, numCherries: Int, inCone: Boolean)

object Encoders {
  implicit val employeeEncoder: CsvEncoder[Employee] =
    new CsvEncoder[Employee] {
      def encode(e: Employee): List[String] =
        List(
          e.name,
          e.number.toString,
          if (e.manager) "yes" else "no"
        )
    }

  implicit val iceCreamEncoder: CsvEncoder[IceCream] =
    new CsvEncoder[IceCream] {
      def encode(i: IceCream): List[String] =
        List(
          i.name,
          i.numCherries.toString,
          if (i.inCone) "yes" else "no"
        )
    }

  implicit def pairEncoder[A, B](
    implicit
    aEncoder: CsvEncoder[A],
    bEncoder: CsvEncoder[B]
  ): CsvEncoder[(A, B)] =
    new CsvEncoder[(A, B)] {
      override def encode(pair: (A, B)): List[String] = {
        val (a, b) = pair
        aEncoder.encode(a) ++ bEncoder.encode(b)
      }
    }
}

object CsvEncoderOps {
  def writeCsv[A](values: List[A])(implicit encoder: CsvEncoder[A]): String =
    values.map(value => encoder.encode(value).mkString(",")).mkString("\n")
}

object Demo extends App {
  import Encoders._
  import CsvEncoderOps._

  val employees: List[Employee] = List(
    Employee("Bill", 1, true),
    Employee("Peter", 2, false),
    Employee("Milton", 3, false)
  )

  val iceCreams: List[IceCream] = List(
    IceCream("Sundae", 1, false),
    IceCream("Cornetto", 0, true),
    IceCream("Banana Split", 0, false)
  )

  println(writeCsv(employees))
  println("")
  println(writeCsv(iceCreams))
  println("")
  println(writeCsv(employees zip iceCreams))
}
