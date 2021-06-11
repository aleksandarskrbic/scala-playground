package fp.foundations.chapter3.processing

import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, ExecutionContext, Future }

// For example, here is a ParList[Int] with two partitions:
// ParList(
//  List(1,2,3,4,5,6,7,8), // partition 1
//  List(9,10)             // partition 2
// )
// Note that we used the `apply` method with a varargs argument.
case class ParList[A](partitions: List[List[A]]) {
  def toList: List[A] = partitions.flatten

  def map[To](fn: A => To): ParList[To] =
    ParList(partitions.map(_.map(fn)))

  def foldLeft[To](default: To)(combine: (To, A) => To): To =
    sys.error("Impossible")

  def foldLeftV2[To](default: To)(combineElement: (To, A) => To)(combinePartition: (To, To) => To): To =
    partitions
      .map(_.foldLeft(default)(combineElement))
      .foldLeft(default)(combinePartition)

  def monoFoldLeftV1(default: A)(combine: (A, A) => A): A =
    partitions
      .map(_.foldLeft(default)(combine))
      .foldLeft(default)(combine)

  def monoFoldLeft(param: Monoid[A]): A =
    partitions.map(_.foldLeft(param.default)(param.combine)).foldLeft(param.default)(param.combine)

  // same as map reduce
  def foldMap[To](fn: A => To)(monoid: Monoid[To]): To =
    partitions.map {
      p => p.foldLeft(monoid.default)((state, value) => monoid.combine(state, fn(value)))
    }.foldLeft(monoid.default)(monoid.combine)
}

object ParList {
  // The `*` at the end of List[A] is called a varargs. It means we can put as many arguments
  // as we want of type List[A] and the Scala compiler will automatically packs these arguments
  // into a collection.
  // For example, ParList(List(1,2), List(3,4)) == ParList(List(List(1,2), List(3,4)))
  // This is why we can create a List using the syntax List(1,2,3) instead of 1 :: 2 :: 3 :: Nil
  def apply[A](partitions: List[A]*): ParList[A] =
    ParList(partitions.toList)

  // Creates a ParList by grouping a List into partitions of fixed size.
  // If the length of input list is not divisible by the partition size, then
  // the last partition will be smaller. For example:
  // byPartitionSize(3, List(1,2,3,4,5,6,7,8,9,10)) == ParList(
  //   List(1,2,3),
  //   List(4,5,6),
  //   List(7,8,9),
  //   List(10)
  // )
  def byPartitionSize[A](partitionSize: Int, items: List[A]): ParList[A] =
    if (items.isEmpty) ParList()
    else ParList(items.grouped(partitionSize).toList)
}
