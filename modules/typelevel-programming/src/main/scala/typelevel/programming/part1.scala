package typelevel.programming

import typelevel.programming.part1.Natural

object part1 {

  import scala.reflect.runtime.universe._
  def show[T](value: T)(implicit tag: TypeTag[T]) =
    tag.toString().replace("typelevel.programming.part1", "")

  // type-level programming

  // represents natural number as TYPE
  // peano arithmetic
  trait Natural
  class _0                      extends Natural
  class Successor[N <: Natural] extends Natural

  type _1 = Successor[_0]
  type _2 = Successor[_1] // Successor[Successor[_0]]
  type _3 = Successor[_2]
  type _4 = Successor[_3]
  type _5 = Successor[_4]

  // type Dog = Successor[String] -> Compiler error:
  // type arguments [String] do not conform to class Successor's type parameter bounds [N <: typelevel.programming.part1.Natural]

  // is _2 < _3 ???
  trait <[A <: Natural, B <: Natural]

  object < {
    implicit def less[B <: Natural]: <[_0, Successor[B]] =
      new <[_0, Successor[B]] {}

    implicit def inductive[A <: Natural, B <: Natural](implicit lt: <[A, B]): <[Successor[A], Successor[B]] =
      new <[Successor[A], Successor[B]] {}

    def apply[A <: Natural, B <: Natural](implicit less: <[A, B]): A < B = less // like implicitly
  }

  val comparison1: <[_0, _1] = <[_0, _1]
  val comparison2: _0 < _1   = <[_0, _1]
  // val comparison3: <[_1, _2] = <[_1, _2] -> Can't compile no implicit found!

  // after implementing inductive
  val comparison3: <[_1, _2] = <[_1, _2]

  def main(args: Array[String]): Unit =
    println(show(List(1, 2, 3)))
}
