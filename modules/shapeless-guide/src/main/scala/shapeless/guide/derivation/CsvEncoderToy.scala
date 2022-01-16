package shapeless.guide.derivation

trait CsvEncoderToy[A] {
  def encode(value: A): List[String]
}

object CsvEncoderToy {
  def writeCsv[A](values: List[A])(implicit encoder: CsvEncoderToy[A]): String =
    values.map(value => encoder.encode(value).mkString(",")).mkString("\n")
}
