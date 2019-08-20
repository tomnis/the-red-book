package exercises

/**
  *
  * Created by tdm on 2019-08-19.
  */
sealed trait Process[I, O] {

  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) => s match {
      case Cons(h, t) => recv(Some(h()))(t())
      case xs => recv(None)(xs)
    }
    case Emit(h: O, t: Process[I, O]) => Cons(() => h, () => t(s))
  }

  def repeat: Process[I,O] = {
    def go(p: Process[I,O]): Process[I,O] = p match {
      case Halt() => go(this)
      case Await(recv) => Await {
        case None => recv(None)
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }
    go(this)
  }



  def filter[I](p: I => Boolean): Process[I,I] = Await[I,I] {
    case Some(i) if p(i) => Emit(i)
    case _ => Halt()
  }.repeat

  def sum: Process[Double,Double] = {
    def go(acc: Double): Process[Double,Double] =
      Await {
        case Some(d) => Emit(d+acc, go(d+acc))
        case None => Halt()
      }

    go(0.0)
  }

  // 15.1
  def take[I](n: Int): Process[I,I] = {
    if (n <= 0) Halt()
    else Await(i => Emit(i, take[I](n-1)))
  }

  def drop[I](n: Int): Process[I,I] = {
    if (n <= 0) id
    else Await(_ => drop(n - 1))
  }

  def takeWhile[I](f: I => Boolean): Process[I,I] =
    await(i =>
      if (f(i)) emit(i, takeWhile(f))
      else      Halt())


  import Process._
  def id[I]: Process[I, I] = lift(identity)


  // 15.5
  def |>[O2](p2: Process[O,O2]): Process[I,O2] = p2 match {
    case Halt() => Halt()
    case Emit(h, t) => Emit(h, this |> t)
    case Await(f) => this match {
      case Emit(h,t) => t |> f(Some(h))
      case Halt() => Halt() |> f(None)
      case Await(g) => Await((i: Option[I]) => g(i) |> p2)
    }
  }

}
case class Emit[I,O](head: O,
                     tail: Process[I,O] = Halt[I,O]()) extends Process[I,O]

case class Await[I,O](recv: Option[I] => Process[I,O]) extends Process[I,O]

case class Halt[I,O]() extends Process[I,O]


object Process {

  /**
    * A helper function to await an element or fall back to another process
    * if there is no input.
    */
  def await[I,O](f: I => Process[I,O],
                 fallback: Process[I,O] = Halt[I,O]()): Process[I,O] =
    Await[I,O] {
      case Some(i) => f(i)
      case None => fallback
    }

  def liftOne[I,O](f: I => O): Process[I,O] = Await {
    case Some(i) => Emit(f(i))
    case None => Halt()
  }

  def lift[I,O](f: I => O): Process[I,O] = liftOne(f).repeat
}