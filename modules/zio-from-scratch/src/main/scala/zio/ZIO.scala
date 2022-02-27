package zio

sealed trait ZIO[+A] { self =>

  // callback or continuation
  def run(callback: A => Unit): Unit

  def zip[B](that: ZIO[B]): ZIO[(A, B)] =
    ZIO.Zip(self, that)

  def map[B](f: A => B): ZIO[B] =
    ZIO.Map(self, f)

  def steps: Int = self match {
    case ZIO.Effect(_)      => 1
    case ZIO.Succeed(_)     => 1
    case zio: ZIO.Zip[_, _] => zio.left.steps + zio.right.steps
  }
}

object ZIO {
  // call-by-name function
  def succeed[A](value: => A): ZIO[A] =
    ZIO.Effect(() => value)

  def succeedNow[A](value: A): ZIO[A] =
    ZIO.Succeed(value)

  case class Succeed[A](value: A) extends ZIO[A] {
    override def run(callback: A => Unit): Unit =
      callback(value)
  }

  case class Effect[A](f: () => A) extends ZIO[A] {
    override def run(callback: A => Unit): Unit =
      callback(f())
  }

  case class Zip[A, B](left: ZIO[A], right: ZIO[B]) extends ZIO[(A, B)] {
    override def run(callback: ((A, B)) => Unit): Unit =
      left.run(a => right.run(b => callback(a, b)))
  }

  case class Map[A, B](zio: ZIO[A], f: A => B) extends ZIO[B] {
    override def run(callback: B => Unit): Unit =
      zio.run(a => callback(f(a)))
  }

}
