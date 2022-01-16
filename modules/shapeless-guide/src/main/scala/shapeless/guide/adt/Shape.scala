package shapeless.guide.adt

import shapeless.Generic

sealed trait Shape

object Shape extends App {
  final case class Circle(radius: Double)                   extends Shape
  final case class Rectangle(width: Double, height: Double) extends Shape

  def area(shape: Shape): Double =
    shape match {
      case Rectangle(w, h) => w * h
      case Circle(r)       => math.Pi * r * r
    }

  val gen = Generic[Shape]

  val rectangle = gen.to(Rectangle(3.0, 4.0))
  val circle    = gen.to(Circle(1.0))

  println(rectangle)
  println(circle)
}
