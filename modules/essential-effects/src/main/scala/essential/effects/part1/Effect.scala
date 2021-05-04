package essential.effects.part1

case class Effect[A](unsafeRun: () => A) {
  def map[B](f: A => B): Effect[B] =
    Effect(() => f(unsafeRun()))

  def flatMap[B](f: A => Effect[B]): Effect[B] =
    Effect(() => f(unsafeRun()).unsafeRun())
}

object Effect {
  def putStr(s: => String): Effect[Unit] =
    Effect(() => println(s))
}
