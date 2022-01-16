package shapeless.guide.derivation

import shapeless.{ ::, HList, HNil }

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
    instance(hnil => Nil)

  implicit def hlistEncoder[H, T <: HList](
    implicit
    hEncoder: CsvEncoder[H],
    tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :: T] =
    instance {
      case h :: t => hEncoder.encode(h) ++ tEncoder.encode(t)
    }
}

object Demo3 extends App {
  import CsvEncoderTake3._

  val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] =
    implicitly

  println(reprEncoder.encode("abc" :: 123 :: true :: HNil))
}
