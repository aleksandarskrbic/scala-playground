package shapeless.guide.derivation

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
}

object Demo extends App {}
