package zio

import scala.concurrent.ExecutionContext

trait Fiber[+A] {
  def start(): Unit

  // 1. Early completion
  // 2. Late completion
  def join: ZIO[A]
}

class FiberImpl[A](zio: ZIO[A]) extends Fiber[A] {
  var maybeResult: Option[A]    = None
  var callbacks: List[A => Any] = List.empty[A => Any]

  override def start(): Unit =
    ExecutionContext.global.execute { () =>
      zio.run { a =>
        maybeResult = Some(a)
        callbacks.foreach(callback => callback(a))
      }
    }

  override def join: ZIO[A] =
    maybeResult match {
      case Some(a) => ZIO.succeedNow(a)
      case None    => ZIO.async(complete => callbacks = complete :: callbacks)
    }
}

sealed trait ZIO[+A] { self =>
  // callback or continuation
  final def run(callback: A => Unit): Unit = {
    type Erased = ZIO[Any]
    type Cont   = Any => Erased

    def erase(zio: ZIO[A]): Erased = zio

    val stack      = new scala.collection.mutable.Stack[Cont]()
    var currentZIO = erase(self)
    var loop       = true

    def complete(value: Any): Unit =
      if (stack.isEmpty) {
        loop = false
        callback(value.asInstanceOf[A])
      } else {
        val cont = stack.pop()
        currentZIO = cont(value)
      }

    while (loop) {
      currentZIO match {
        case ZIO.Succeed(value) =>
          complete(value)

        case ZIO.Effect(thunk) =>
          complete(thunk())

        case ZIO.FlatMap(zio, cont) =>
          stack.push(cont)
          currentZIO = zio

        case ZIO.Async(register) => ???
        case ZIO.Fork(zio)       => ???
      }
    }
  }

  def repeat(n: Int): ZIO[Unit] =
    if (n <= 0) ZIO.succeedNow(())
    else self *> repeat(n - 1)

  // must be by-name because of stack safety
  def zipWith[B, C](that: => ZIO[B])(f: (A, B) => C): ZIO[C] =
    for {
      a <- self
      b <- that
    } yield f(a, b)

  def zipPar[B](that: ZIO[B]): ZIO[(A, B)] =
    for {
      fa <- self.fork
      fb <- that.fork
      a  <- fa.join
      b  <- fb.join
    } yield (a, b)

  // must be by-name because of stack safety
  def zip[B](that: => ZIO[B]): ZIO[(A, B)] =
    zipWith(that)(_ -> _)

  // must be by-name because of stack safety
  def zipRight[B](that: => ZIO[B]): ZIO[B] =
    zipWith(that)((_, b) => b)

  // must be by-name because of stack safety
  def *>[B](that: => ZIO[B]): ZIO[B] =
    self zipRight that

  def map[B](f: A => B): ZIO[B] =
    flatMap(a => ZIO.succeedNow(f(a)))

  def as[B](value: => B): ZIO[B] =
    self.map(_ => value)

  def flatMap[B](f: A => ZIO[B]): ZIO[B] =
    ZIO.FlatMap(self, f)

  def fork: ZIO[Fiber[A]] =
    ZIO.Fork(self)
}

object ZIO {
  // call-by-name function
  def succeed[A](value: => A): ZIO[A] =
    ZIO.Effect(() => value)

  def succeedNow[A](value: A): ZIO[A] =
    ZIO.Succeed(value)

  def async[A](register: (A => Any) => Any): ZIO[A] =
    ZIO.Async(register)

  case class Succeed[A](value: A) extends ZIO[A]

  case class Effect[A](f: () => A) extends ZIO[A]

  case class FlatMap[A, B](zio: ZIO[A], f: A => ZIO[B]) extends ZIO[B]

  case class Async[A](register: (A => Any) => Any) extends ZIO[A]

  case class Fork[A](zio: ZIO[A]) extends ZIO[Fiber[A]]

}
