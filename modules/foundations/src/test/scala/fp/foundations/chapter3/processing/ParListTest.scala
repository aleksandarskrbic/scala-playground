package fp.foundations.chapter3.processing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import TemperatureExercises._
import fp.foundations.chapter3.processing.model.Sample
import org.scalacheck.Gen

class ParListTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with ParListTestInstances {

  test("minSampleByTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(
      minSampleByTemperature(parSamples) ==
        Some(Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1))
    )
  }

  test("minSampleByTemperature returns the coldest Sample") {
    forAll { (samples: List[Sample]) =>
      val parSamples = ParList.byPartitionSize(3, samples)

      for {
        coldest <- minSampleByTemperature(parSamples)
        sample  <- samples
      } assert(coldest.temperatureFahrenheit <= sample.temperatureFahrenheit)
    }
  }

  test("averageTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(averageTemperature(parSamples) == Some(53.6))
  }

  test("averageTemperature: avg(x - avg) = 0 ") {
    forAll { (samples: ParList[Sample]) =>
      TemperatureExercises.averageTemperature(samples) match {
        case None => succeed
        case Some(avg) =>
          val updated = samples.map(sample => sample.copy(temperatureFahrenheit = sample.temperatureFahrenheit - avg))
          TemperatureExercises.averageTemperature(updated) match {
            case None            => fail("updated average shouldn't be None")
            case Some(updateAvg) => assert(updateAvg.abs <= 0.0001)
          }
      }
    }
  }

  test("monoFoldLeft consistent with List sum") {
    forAll { (numbers: ParList[Int]) =>
      assert(numbers.monoFoldLeft(MonoFoldParam.sumInt) == numbers.toList.sum)
    }
  }

/*  test("monoFoldLeft consistent with List foldLeft (not true)") {
    forAll { (numbers: ParList[Int]) =>
      assert(numbers.monoFoldLeft(0)(_ + _) == numbers.toList.foldLeft(0)(_ + _))
    }
  }*/

  val genInt: Gen[Int]              = Gen.choose(Int.MinValue, Int.MaxValue)
  val genDouble: Gen[Double]        = Gen.choose(Float.MinValue, Float.MaxValue).map(x => x: Double)
  val genString: Gen[String]        = Gen.alphaNumStr
  val genMap: Gen[Map[String, Int]] = Gen.mapOf(Gen.zip(genString, genInt))

  ignore("summary is consistent between implementations") {
    forAll { (samples: ParList[Sample]) =>
      val samplesList = samples.partitions.flatten
      val reference   = summaryList(samples.partitions.flatten)
      List(
        summaryListOnePass(samplesList),
        summaryParList(samples),
        summaryParListOnePass(samples)
      ).foreach { other =>
        assert(reference.size == other.size)
        assert((reference.sum - other.sum).abs < 0.00001)
        assert(reference.min == other.min)
        assert(reference.max == other.max)
      }
    }
  }
}
