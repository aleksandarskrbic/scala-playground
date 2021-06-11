package fp.foundations.chapter2

import fp.foundations.chapter2.GenericFunctionExercises._
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import org.scalacheck.{ Arbitrary, Gen }
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Try

class GenericFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  test("Pair swap") {
    assert(Pair(0, 1).swap == Pair(1, 0))
  }

  test("Pair map") {
    assert(Pair(0, 1).map(identity) == Pair(0, 1))
  }

  test("Pair decoded") {
    assert(decoded == Pair("Functional", "Programming"))
  }

  test("Pair zipWith") {
    assert(Pair(0, 1).zipWith(Pair(2, 3))(_ + _) == Pair(2, 4))
  }

  test("Pair productNames") {}

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  test("Predicate && examples") {
    assert((isEven && isPositive)(12))
    assert(!(isEven && isPositive)(11))
    assert(!(isEven && isPositive)(-4))
    assert(!(isEven && isPositive)(-7))
  }

  test("Predicate &&") {
    forAll { (p: Int => Boolean, number: Int) =>
      val predicate = Predicate(p)
      assert(!(predicate && Predicate.False)(number))
      assert((predicate && Predicate.True)(number) == predicate(number))
    }
  }

  test("Predicate ||") {
    forAll { (p: Int => Boolean, number: Int) =>
      val predicate = Predicate(p)
      assert((predicate || Predicate.True)(number))
      assert((predicate || Predicate.False)(number) == predicate(number))
    }
  }

  test("Predicate flip") {
    assert(!Predicate.True.flip(()))
    assert(Predicate.False.flip(()))
  }

  test("Predicate isValidUser") {
    assert(isValidUser(User("John", 20)))
    assert(!isValidUser(User("John", 17)))
    assert(!isValidUser(User("john", 20)))
    assert(!isValidUser(User("x", 23)))
  }

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {
    assert(userIdDecoder.decode("1234") == UserId(1234))
    assert(userIdDecoder.decode("-1") == UserId(-1))

    assert(Try(userIdDecoder.decode("hello")).isFailure)
    assert(Try(userIdDecoder.decode("1111111111111111")).isFailure)
  }

  test("JsonDecoder UserId round-trip") {
    forAll((id: Int) => assert(userIdDecoder.decode(id.toString) == UserId(id)))
  }

  test("JsonDecoder LocalDate") {
    assert(localDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020, 3, 26))
    assert(Try(localDateDecoder.decode("2020-03-26")).isFailure)
    assert(Try(localDateDecoder.decode("hello")).isFailure)
  }

  test("JsonDecoder weirdLocalDateDecoder") {}

  val localDateGen: Gen[LocalDate] =
    Gen
      .choose(LocalDate.MIN.toEpochDay, LocalDate.MAX.toEpochDay)
      .map(LocalDate.ofEpochDay)

  implicit val localDateArbitrary: Arbitrary[LocalDate] =
    Arbitrary(localDateGen)
}
