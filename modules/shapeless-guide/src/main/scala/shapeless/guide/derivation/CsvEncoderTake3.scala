package shapeless.guide.derivation

import shapeless.{ ::, Generic, HList, HNil }

object CsvEncoderTake3 {
  def apply[A](implicit encoder: CsvEncoder[A]): CsvEncoder[A] =
    encoder

  def instance[A](fn: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
      override def encode(value: A): List[String] =
        fn(value)
    }

  implicit val stringEncoder: CsvEncoder[String] =
    instance(str => List(str))

  implicit val intEncoder: CsvEncoder[Int] =
    instance(num => List(num.toString))

  implicit val booleanEncoder: CsvEncoder[Boolean] =
    instance(bool => List(if (bool) "yes" else "no"))

  implicit val hnilEncoder: CsvEncoder[HNil] =
    instance(_ => Nil)

  implicit def hlistEncoder[H, T <: HList](
    implicit
    hEncoder: CsvEncoder[H],
    tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :: T] =
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
    enc: CsvEncoder[R]
  ): CsvEncoder[A] =
    instance(a => enc.encode(gen.to(a)))
}

object Demo3 extends App {
  import CsvEncoderTake3._
  import shapeless.guide.derivation.CsvEncoder.writeCsv

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

  val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly

  println(reprEncoder.encode("abc" :: 123 :: true :: HNil))
  println()

  // writeCsv(iceCreams)

  val csv = writeCsv(iceCreams)(
    genericEncoder(
      Generic[IceCream],
      hlistEncoder(stringEncoder, hlistEncoder(intEncoder, hlistEncoder(booleanEncoder, hnilEncoder)))
    )
  )

  println(csv)
}
