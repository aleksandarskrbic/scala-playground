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
}

case object RNil extends RList[Nothing] {
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
}

object test extends App {
  val list1 = ::(1, ::(2, ::(3, RNil)))
  val list2 = 1 :: 2 :: 3 :: RNil

  println(list1)
  println(list1(1))
  println(list2.length)
  println(list1.reverse)
}
