package fp.foundations.chapter3.processing

/**
 * Use a default value such as when used with combine it is no-operation
 * */
trait MonoFoldParam[A] {

  def default: A

  def combine(first: A, second: A): A

}

object MonoFoldParam {
  val sumInt: MonoFoldParam[Int] = new MonoFoldParam[Int] {
    override def default: Int                          = 0
    override def combine(first: Int, second: Int): Int = first + second
  }
}
