package problems

import scala.annotation.tailrec

sealed abstract class RList[+T] { self =>
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): RList[S] = new ::(elem, self)

  def apply(index: Int): T

  def length: Int = {
    @tailrec def loop(remaining: RList[T], size: Int): Int =
      if (remaining.isEmpty) size
      else loop(remaining.tail, size + 1)

    loop(self, 0)
  }

  def reverse: RList[T] = {
    @tailrec def loop(remaining: RList[T], result: RList[T]): RList[T] =
      if (remaining.isEmpty) result
      else loop(remaining.tail, remaining.head :: result)

    loop(self, RNil)
  }

  def ++[S >: T](other: RList[S]): RList[S] = {
    @tailrec
    def loop(remaining: RList[S], result: RList[S]): RList[S] =
      if (remaining.isEmpty) result
      else loop(remaining.tail, remaining.head :: result)

    loop(self.reverse, other)
  }

  def removeAt(index: Int): RList[T]

  def map[S](fn: T => S): RList[S]
  def flatMap[S](fn: T => RList[S]): RList[S]
  def filter(fn: T => Boolean): RList[T]

  // run-length-encoding
  def rle: RList[(T, Int)]
  def duplicateEach(n: Int): RList[T]
  // rotate each element n positions to left
  def rotate(n: Int): RList[T]
}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec def loop(remaining: Iterable[T], result: RList[T]): RList[T] =
      if (remaining.isEmpty) result
      else loop(remaining.tail, remaining.head :: result)

    loop(iterable, RNil).reverse
  }

  def from[T](elem: T): RList[T] =
    elem :: RNil
}

case object RNil extends RList[Nothing] { self =>
  override def head: Nothing =
    throw new NoSuchElementException

  override def tail: RList[Nothing] =
    throw new NoSuchElementException

  override def isEmpty: Boolean =
    true

  override def toString: String =
    "[]"

  override def apply(index: Int): Nothing =
    throw new NoSuchElementException

  override def removeAt(index: Int): RList[Nothing] =
    self

  override def map[S](fn: Nothing => S): RList[S] =
    RNil

  override def flatMap[S](fn: Nothing => RList[S]): RList[S] =
    RNil

  override def filter(fn: Nothing => Boolean): RList[Nothing] =
    RNil

  override def rle: RList[(Nothing, Int)] =
    RNil

  override def duplicateEach(n: Int): RList[Nothing] =
    RNil

  override def rotate(n: Int): RList[Nothing] =
    RNil
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] { self =>
  override def isEmpty: Boolean =
    false

  override def toString: String = {
    @tailrec def loop(remaining: RList[T], result: String): String =
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else loop(remaining.tail, s"$result${remaining.head}, ")

    "[" + loop(self, "") + "]"
  }

  override def apply(index: Int): T = {
    @tailrec def loop(remaining: RList[T], currentIndex: Int): T =
      if (currentIndex == index) remaining.head
      else loop(remaining.tail, currentIndex + 1)

    if (index < 0) throw new NoSuchElementException
    else loop(self, 0)
  }

  override def removeAt(index: Int): RList[T] = {
    @tailrec def loop(remaining: RList[T], result: RList[T], currentIndex: Int): RList[T] =
      if (remaining.isEmpty) result
      else if (currentIndex == index) result ++ remaining.tail
      else loop(remaining.tail, remaining.head :: result, currentIndex + 1)

    if (index < 0) self
    else if (index == 0) tail
    else loop(self, RNil, 0).reverse
  }

  override def map[S](fn: T => S): RList[S] = {
    @tailrec def loop(remaining: RList[T], result: RList[S]): RList[S] =
      if (remaining.isEmpty) result
      else loop(remaining.tail, fn(remaining.head) :: result)

    loop(self.reverse, RNil)
  }

  override def flatMap[S](fn: T => RList[S]): RList[S] = {
    @tailrec def loop(remaining: RList[T], result: RList[S]): RList[S] =
      if (remaining.isEmpty) result
      else loop(remaining.tail, fn(remaining.head) ++ result)

    loop(self.reverse, RNil)
  }

  override def filter(fn: T => Boolean): RList[T] = {
    @tailrec def loop(remaining: RList[T], result: RList[T]): RList[T] =
      if (remaining.isEmpty) result
      else {
        val currentElement = remaining.head
        if (fn(currentElement)) loop(remaining.tail, currentElement :: result)
        else loop(remaining.tail, result)
      }

    loop(self, RNil)
  }

  override def rle: RList[(T, Int)] = {
    @tailrec def loop(remaining: RList[T], currentPair: (T, Int), accumulator: RList[(T, Int)]): RList[(T, Int)] =
      if (remaining.isEmpty) currentPair :: accumulator
      else if (remaining.head == currentPair._1) loop(remaining.tail, (currentPair._1, currentPair._2 + 1), accumulator)
      else loop(remaining.tail, (remaining.head, 1), currentPair :: accumulator)

    loop(self.tail, (self.head, 1), RNil).reverse
  }

  override def duplicateEach(n: Int): RList[T] = {
    @tailrec def loop(remaining: RList[T], currentCount: Int, result: RList[T]): RList[T] =
      if (remaining.isEmpty) result
      else if (currentCount < n) loop(remaining, currentCount + 1, remaining.head :: result)
      else loop(remaining.tail, 0, result)

    loop(self, 0, RNil).reverse
  }

  override def rotate(n: Int): RList[T] = {
    @tailrec def split(remaining: RList[T], result: RList[T]): RList[T] =
      if (result.length == n) remaining ++ result.reverse
      else split(remaining.tail, remaining.head :: result)

    split(self, RNil)
  }
}

object test extends App {
  val list = 1 :: 2 :: 3 :: RNil

  /*  println(list)
  println(list(1))

  println(list.length)
  println(list.reverse)

  println(RList.from(List(3, 4, 5)))

  val concatenated = RList.from(List(1, 2, 3)) ++ RList.from(List(4, 5, 6))
  println(concatenated)*/

  /*  println(list.removeAt(2))
  println(list.removeAt(0))*/

  /*
  println(list.map(_ * 2))
  println(list.flatMap(e => RList.from(List(e, e * 2))))
  println(list.filter(_ % 2 == 0))
   */

  println(RList.from(List(1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 5, 5, 6, 7)).rle)
  RList.from(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  println(list.duplicateEach(3))
  println(RList.from(List(1, 2, 3, 4, 5, 6, 7, 8, 9)).rotate(3))
  println(RList.from(1 to 10000).rotate(100))
}
