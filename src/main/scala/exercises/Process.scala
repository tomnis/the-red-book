package exercises

/**
  *
  * Created by tdm on 2019-08-19.
  */
sealed trait Process[I, O] {

  import Process._

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



  def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)


  def ++(next: => Process[I, O]): Process[I, O] = this match {
    case Halt() => next
    case Emit(head, tail) => Emit(head, tail ++ next)
    case Await(recv) => Await(recv andThen (_ ++ next))
  }

  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
    case Halt() => Halt()
    case Emit(head, tail) => f(head) ++ tail.flatMap(f)
    case Await(recv) => Await(recv andThen(_ flatMap f))
  }


  def monad[I]: Monad[({type f[x] = Process[I, x]})#f] = new Monad[({type f[x] = Process[I, x]})#f] {
    def unit[O](o: => O): Process[I, O] = Emit(o)

    def flatMap[O, O2](p: Process[I, O])(f: O => Process[I, O2]): Process[I, O2] = p flatMap f
  }
}


/**
  * Indicates to the driver that `head` should be emitted to the output stream, and machine should be transitioned
  * to the `tail` state.
  *
  * @param head
  * @param tail
  * @tparam I
  * @tparam O
  */
case class Emit[I,O](head: O,
                     tail: Process[I,O] = Halt[I,O]()) extends Process[I,O]


/**
  * Requests a value from the input stream.
  *
  * @param recv
  * @tparam I
  * @tparam O
  */
case class Await[I,O](recv: Option[I] => Process[I,O]) extends Process[I,O]


/**
  * No more elements should be read from input or emitted to output.
  * @tparam I
  * @tparam O
  */
case class Halt[I,O]() extends Process[I,O]


object Process {

  def id[I]: Process[I, I] = lift(identity)

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
    else await(i => Emit(i, take[I](n-1)))
  }

  def drop[I](n: Int): Process[I,I] = {
    if (n <= 0) id
    else Await(_ => drop(n - 1))
  }

  def takeWhile[I](f: I => Boolean): Process[I,I] =
    await(i =>
      if (f(i)) Emit(i, takeWhile(f))
      else Halt()
    )


  def dropWhile[I](f: I => Boolean): Process[I, I] = {
    await(i =>
      if (f(i)) dropWhile(f)
      else Emit(i, id)
    )
  }



  // 15.2
  def count[I]: Process[I, Int] = {
    def go(acc: Int): Process[I, Int] = Await {
      case Some(_) => Emit(acc + 1, go(acc + 1))
      case None => Halt()
    }

    go(0)
  }


  // 15.3
  def mean: Process[Double, Double] = {
    def go(oldMean: Double, oldCount: Int): Process[Double, Double] = Await {
      case Some(d) => {
        val newCount: Int = oldCount + 1
        val newMean: Double = (oldMean * oldCount + d) / newCount
        Emit(newMean, go(newMean, newCount))
      }
      case None => Halt()
    }

    go(0, 0)
  }



  // 15.4
  def sumLoop: Process[Double, Double] = loop(0.0)((i, acc) => (i + acc, i + acc))
  def countLoop[I]: Process[I, Int] = loop(0)((_, acc) => (acc + 1, acc + 1))

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



  def loop[S, I, O](zero: S)(f: (I, S) => (O, S)): Process[I, O] = {
    await((i: I) => f(i, zero) match {
      case (o, acc) => Emit(o, loop(acc)(f))
    })
  }



  // 15.7
  def zipWithIndex[I]: Process[I, (I, Int)] = {
    loop(-1)((i, acc) => ((i,acc + 1), acc + 1))
  }


  // 15.8
  def exists[I](f: I => Boolean): Process[I, Boolean] = {
    await(i => if(f(i)) Emit(true) else exists(f))
  }
}