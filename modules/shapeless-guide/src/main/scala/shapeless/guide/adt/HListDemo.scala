package shapeless.guide.adt

import shapeless.{ Generic, HNil }

case class IceCream(name: String, numCherries: Int, inCone: Boolean)
case class Employee(name: String, number: Int, manager: Boolean)

object HListDemo extends App {
  val product    = "Sunday" :: 1 :: false :: HNil
  val newProduct = 42L :: product

  val iceCreamGen = Generic[IceCream]

  val iceCream = IceCream("Sundae", 1, false)

  val repr      = iceCreamGen.to(iceCream)
  val iceCream2 = iceCreamGen.from(repr)

  println(repr)
  println(iceCream2)

  val employee = Generic[Employee].from(Generic[IceCream].to(iceCream))

  println(employee)

}
