package shapeless.guide.derivation.test

import shapeless.guide.adt.Shape
import shapeless.guide.adt.Shape._
import shapeless.guide.derivation.test.CsvEncoderToy.writeCsv
import shapeless.{ :+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr }

object CsvEncoderTake4 {
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

  implicit def genericEncoder[A, R](
    implicit
    gen: Generic.Aux[A, R],
    enc: CsvEncoderToy[R]
  ): CsvEncoderToy[A] =
    instance(a => enc.encode(gen.to(a)))

  implicit val cnilEncoder: CsvEncoderToy[CNil] =
    instance(_ => throw new Exception("Inconceivable!"))

  implicit val doubleEncoder: CsvEncoderToy[Double] =
    instance(d => List(d.toString))

  implicit def coproductEncoder[H, T <: Coproduct](
    implicit
    hEncoder: CsvEncoderToy[H],
    tEncoder: CsvEncoderToy[T]
  ): CsvEncoderToy[H :+: T] = instance {
    case Inl(h) => hEncoder.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }
}

object Demo4 extends App {
  import CsvEncoderTake4._

  val shapes: List[Shape] = List(Rectangle(3.0, 4.0), Circle(1.0))

  println(writeCsv(shapes))
}
