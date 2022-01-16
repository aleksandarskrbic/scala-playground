package shapeless.guide.derivation

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}
