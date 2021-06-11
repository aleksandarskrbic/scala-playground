package fp.foundations.chapter3.processing.model

case class Summary(
  min: Option[Sample], // Sample with lowest temperature
  max: Option[Sample], // Sample with highest temperature
  sum: Double,         // sum of all temperatures in Fahrenheit
  size: Int            // number of Samples
) {

  def average: Option[Double] =
    if (size == 0) None
    else Some(sum / size)

  override def toString: String =
    f"Summary(avg = ${average.getOrElse(0.0)}%.2f, " +
      s"size = $size,\n  " +
      s"min = $min,\n  " +
      s"max = $max\n)"
}
