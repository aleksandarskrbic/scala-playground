package fp.foundations.chapter3.processing

import fp.foundations.chapter3.processing.model.{Sample, Summary, SummaryV1}
import fp.foundations.chapter3.processing.util.ThreadPoolUtil
import fp.foundations.chapter3.processing.util.TimeUtil.{Labelled, bench, timeOne}
import kantan.csv.ops.toCsvInputOps
import kantan.csv.{CsvReader, ReadError, rfc}

object TemperatureExercises {
  val rawData: java.net.URL = getClass.getResource("/city_temperature.csv")

  val reader: CsvReader[Either[ReadError, Sample]] = rawData.asCsvReader[Sample](rfc.withHeader)

  val (failures, samples) = timeOne("load data")(reader.toList.partitionMap(identity))

  println(s"Parsed ${samples.size} rows successfully and ${failures.size} rows failed")

  val partitions    = 10
  val partitionSize = samples.length / partitions + 1
  val computeEC     = ThreadPoolUtil.fixedSizeExecutionContext(8, "compute")

  val parSamples      = ParList.byNumberOfPartition(computeEC, 10, samples)
  val samplesArray    = samples.toArray
  val parSamplesArray = ParArray(computeEC, samplesArray, partitionSize)

  val coldestSample = TemperatureAnswers.minSampleByTemperature(parSamples)

  println(s"Min sample by temperature is $coldestSample")
  println(s"Max sample by temperature is ${parSamples.maxBy(_.temperatureFahrenheit)}")
  println(s"Min sample by date is ${parSamples.minBy(_.localDate)}")
  println(s"Max sample by date is ${parSamples.maxBy(_.localDate)}")

  val averageTemperature = TemperatureAnswers.averageTemperature(parSamples)

  println(s"Average temperature is $averageTemperature")

  println(s"Temperature summary is ${parSamples.parFoldMap(SummaryV1.one)(SummaryV1.monoid)}")

  sealed trait Label
  case class City(value: String)    extends Label
  case class Country(value: String) extends Label

  val summariesPerCity = aggregatePerLabel(parSamples)(s => List(City(s.city), Country(s.country)))

  summariesPerCity.get(City("Bordeaux")).foreach(println)
  summariesPerCity.get(City("London")).foreach(println)
  summariesPerCity.get(City("Mexico")).foreach(println)
  summariesPerCity.get(Country("London")).foreach(println)

  def aggregatePerLabel[Label](parList: ParList[Sample])(labels: Sample => List[Label]): Map[Label, Summary] = {
    def sampleToLabelledSummary(sample: Sample): Map[Label, Summary] = {
      val summary = Summary.one(sample)
      labels(sample).map(_ -> summary).toMap
    }

    parList.parReduceMap(sampleToLabelledSummary)(Monoid.map(Summary.semigroup)).getOrElse(Map.empty)
  }
}
