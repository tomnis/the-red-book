package exercises

/**
  * Created by tomas.mccandless on 2019-03-18.
  */
trait RNG {
  def nextInt: (Int, RNG)
}


object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  // "a randomly generated a"
  // note this encapsulates RNG (something with nextInt) as an "underlying source of randomness" and relies on external
  // transformations from Int to A
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  // transform the output of an action without further modifying state
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = ???

  def double(rng: RNG): (Double, RNG) = ???

  def intDouble(rng: RNG): ((Int,Double), RNG) = ???

  def doubleInt(rng: RNG): ((Double,Int), RNG) = ???

  def double3(rng: RNG): ((Double,Double,Double), RNG) = ???

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = ???

  // combine 2 rng actions into one
  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng: RNG => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  //6.8
  // generate a random A, then generate a random B based on that value
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng: RNG => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }
}

// why is state not contravariant?
case class StateM[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): StateM[S, B] = {


  }

  def map2[B,C](sb: StateM[S, B])(f: (A, B) => C): StateM[S, C] =
    ???

  def flatMap[B](f: A => StateM[S, B]): StateM[S, B] =
    ???
}


object StateM {


  def unit[S, A]: StateM[S, A]
}