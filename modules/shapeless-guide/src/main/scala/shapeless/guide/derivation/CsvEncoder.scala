package shapeless.guide.derivation

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {
  def writeCsv[A](values: List[A])(implicit encoder: CsvEncoder[A]): String =
    values.map(value => encoder.encode(value).mkString(",")).mkString("\n")
}
