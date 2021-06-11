package fp.foundations.chapter3

object ForLoopExercises extends App {

  case class User(name: String, email: Option[String])
  val users = List(
    User("John", Some("john@foo.com")),
    User("Eda ", None),
    User("Kim", Some("kim@bar.com"))
  )

  for {
    user  <- users
    email <- user.email
  } println(email)

  def sum(numbers: List[Int]): Int = {
    var total = 0
    for (number <- numbers) total += number
    total
  }

  def sumFoldLeft(numbers: List[Int]): Int =
    foldLeft(numbers, 0)(_ + _)

  // a. Implement `size` using a mutable state and a for loop
  // such as size(List(2,5,1,8)) == 4
  // and     size(Nil) == 0
  def size[A](elements: List[A]): Int = {
    var counter = 0
    for (_ <- elements) counter += 1
    counter
  }

  def sizeFoldLeft[A](elements: List[A]): Int =
    foldLeft(elements, 0)((state, _) => state + 1)

  // b. Implement `min` using a mutable state and a for loop
  // such as min(List(2,5,1,8)) == Some(1)
  // and     min(Nil) == None
  // Note: Option is an enumeration with two values:
  // * Some when there is a value and
  // * None when there is no value (a bit like null)
  def min(numbers: List[Int]): Option[Int] = {
    var state = Option.empty[Int]
    for (number <- numbers) state = combineMin(state, number)
    state
  }

  private def combineMin(state: Option[Int], number: Int): Option[Int] =
    state match {
      case None               => Some(number)
      case Some(currentState) => Some(currentState min number)
    }

  def minFoldLeft(numbers: List[Int]): Option[Int] =
    foldLeft(numbers, Option.empty[Int])(combineMin)

  // c. Implement `wordCount` using a mutable state and a for loop.
  // `wordCount` compute how many times each word appears in a `List`
  // such as wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1)
  // and     wordCount(Nil) == Map.empty
  // Note: You can lookup an element in a `Map` with the method `get`
  // and you can upsert a value using `updated`
  def wordCount(words: List[String]): Map[String, Int] = {
    var state = Map.empty[String, Int]

    for (word <- words) {
      val count = state.getOrElse(word, 0)
      state = state.updated(word, count + 1)
    }

    state
  }

  def wordCountFoldLeft(words: List[String]): Map[String, Int] =
    foldLeft(words, Map.empty[String, Int]) { (state, item) =>
      val count = state.getOrElse(item, 0)
      state.updated(item, count + 1)
    }

  // d. `sum`, `size`, `min` and `wordCount` are quite similar.
  // Could you write a higher-order function that captures this pattern?
  // How would you call it?
  def foldLeft[From, To](items: List[From], defaultValue: To)(combine: (To, From) => To): To = {
    var state = defaultValue
    for (item <- items) state = combine(state, item)
    state
  }

  // e. Refactor `sum`, `size`, `min` and `wordCount` using the higher-order
  // function you defined above.

  //////////////////////////////////////////////
  // Bonus question (not covered by the video)
  //////////////////////////////////////////////

  // f. `foldLeft` can be used to implement most of the List API.
  // Do you want to give it a try? For example, can you implement
  // `map`, `reverse` and `lastOption` in terms of `foldLeft`
  def map[From, To](elements: List[From])(update: From => To): List[To] =
    ???

  // reverse(List(3,8,1)) == List(1,8,3)
  // reverse(Nil) == Nil
  def reverse[A](elements: List[A]): List[A] =
    ???

  // lastOption(List(3,8,1)) == Some(1)
  // lastOption(Nil) == None
  def lastOption[A](elements: List[A]): Option[A] =
    ???

  // g. Can you generalise `min` so that it applies to more types like `Long`, `String`, ...?
  // Note: You may want to use the class Ordering from the standard library
  def generalMin[A](elements: List[A]): Option[A] =
    ???

}
