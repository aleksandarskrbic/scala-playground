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
}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec def loop(remaining: Iterable[T], result: RList[T]): RList[T] =
      if (remaining.isEmpty) result
      else loop(remaining.tail, remaining.head :: result)

    loop(iterable, RNil).reverse
  }
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

  override def flatMap[S](fn: T => RList[S]): RList[S] = ???

  override def filter(fn: T => Boolean): RList[T] = ???
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

  println(list.map(_ * 2))
}
