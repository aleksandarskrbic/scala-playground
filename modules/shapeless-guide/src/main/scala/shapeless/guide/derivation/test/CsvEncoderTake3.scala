package shapeless.guide.derivation.test

import shapeless.guide.derivation.Employee
import shapeless.guide.derivation.IceCream
import shapeless.{ ::, Generic, HList, HNil }
import shapeless.guide.derivation.test.CsvEncoderToy.writeCsv

object CsvEncoderTake3 {
  def apply[A](implicit encoder: CsvEncoderToy[A]): CsvEncoderToy[A] =
    encoder

  def instance[A](fn: A => List[String]): CsvEncoderToy[A] =
    new CsvEncoderToy[A] {
      override def encode(value: A): List[String] =
        fn(value)
    }

  implicit val stringEncoder: CsvEncoderToy[String] =
    instance(str => List(str))

  implicit val intEncoder: CsvEncoderToy[Int] =
    instance(num => List(num.toString))

  implicit val booleanEncoder: CsvEncoderToy[Boolean] =
    instance(bool => List(if (bool) "yes" else "no"))

  implicit val hnilEncoder: CsvEncoderToy[HNil] =
    instance(_ => Nil)

  implicit def hlistEncoder[H, T <: HList](
    implicit
    hEncoder: CsvEncoderToy[H],
    tEncoder: CsvEncoderToy[T]
  ): CsvEncoderToy[H :: T] =
    instance {
      case h :: t => hEncoder.encode(h) ++ tEncoder.encode(t)
    }

  /*
  implicit def genericEncoder[A, R](
    implicit
    gen: Generic[A] { type Repr = R },
    enc: CsvEncoder[R]
  ): CsvEncoder[A] =
    instance(a => enc.encode(gen.to(a)))
   */

  implicit def genericEncoder[A, R](
    implicit
    gen: Generic.Aux[A, R],
    enc: CsvEncoderToy[R]
  ): CsvEncoderToy[A] =
    instance(a => enc.encode(gen.to(a)))
}

object Demo3 extends App {
  import CsvEncoderTake3._

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

  val reprEncoder: CsvEncoderToy[String :: Int :: Boolean :: HNil] = implicitly

  println(reprEncoder.encode("abc" :: 123 :: true :: HNil))
  println()

  /*  val csv = writeCsv(iceCreams)(
    genericEncoder(
      Generic[IceCream],
      hlistEncoder(stringEncoder, hlistEncoder(intEncoder, hlistEncoder(booleanEncoder, hnilEncoder)))
    )
  )*/

  println(writeCsv(iceCreams))
}
