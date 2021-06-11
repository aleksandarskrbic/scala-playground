package fp.foundations.chapter1

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import fp.foundations.chapter1.ValueFunctionExercises._

class ValueFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////////////////////////////////
  // Exercise 1: String API with higher-order functions
  /////////////////////////////////////////////////////

  test("selectDigits examples") {
    assert(selectDigits("hello4world-80") == "480")
    assert(selectDigits("welcome") == "")
  }

  test("selectDigits length is equal or smaller") {
    forAll((text: String) => assert(selectDigits(text).length <= text.length))
  }

  test("selectDigits result is subset of a given text") {
    forAll((text: String) => assert(text.contains(selectDigits(text))))
  }

  test("selectDigits will return only digits") {
    forAll((text: String) => selectDigits(text).foreach(c => assert(c.isDigit)))
  }

  test("secret is idempotent") {
    forAll { (text: String) =>
      val once  = secret(text)
      val twice = secret(secret(text))
      assert(once == twice)
    }
  }

  test("isValidUsername") {
    forAll((username: String) => assert(isValidUsername(username) == isValidUsername(username.reverse)))
  }

  ///////////////////////
  // Exercise 2: Point
  ///////////////////////

  test("point isPositive") {
    forAll((x: Int, y: Int, z: Int) => assert(Point(x.max(0), y.max(0), z.max(0)).isPositive))
  }

  test("point forall") {
    forAll { (x: Int, y: Int, z: Int, predicate: Int => Boolean) =>
      assert(Point(x.max(0), y.max(0), z.max(0)).forAll(predicate) == List(x, y, z).forall(predicate))
    }
  }
}
