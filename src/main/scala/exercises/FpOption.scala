package exercises

sealed trait FpOption[+A] {

  def map[B](f: A => B): FpOption[B] = {
    this match {
      case FpSome(a) => FpOption(f(a))
      case FpNone => FpNone
    }
  }


  def flatMap[B](f: A => FpOption[B]): FpOption[B] = {
    this.map(f).getOrElse(FpNone)
  }


  def getOrElse[B >: A](default: => B): B = {
    this match {
      case FpSome(a) => a
      case FpNone => default
    }
  }


  // without matching
  def orElse[B >: A](ob: => FpOption[B]): FpOption[B] = {
    this.map(a => FpOption(a)).getOrElse(ob)
  }


  // without matching
  def filter(f: A => Boolean): FpOption[A] = {
    this.flatMap(a => if (f(a)) FpOption(a) else FpNone)
  }
}


case class FpSome[+A](get: A) extends FpOption[A]
case object FpNone extends FpOption[Nothing]


object FpOption {

  def apply[A](a: A): FpOption[A] = {
    if (a != null) FpSome(a)
    else FpNone
  }






  def divide(a: Int, b: Int): Option[Int] = {
    if (b == 0) None
    else Some(a / b)
  }
}
