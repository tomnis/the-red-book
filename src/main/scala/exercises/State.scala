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

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, s) = rng.nextInt
    i match {
      case Int.MinValue => (Int.MaxValue, s)
      case other => (other, s)
    }
  }


  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, s) = rng.nextInt
    (i / Int.MaxValue.toDouble, s)
  }


  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, s) = rng.nextInt
    val (d,s2) = double(s)
    ((i, d), s2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, s) = double(rng)
    val (i, s2) = s.nextInt
    ((d, i), s2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, s1) = double(rng)
    val (d2, s2) = double(s1)
    val (d3, s3) = double(s2)
    ((d1,d2,d3), s3)
  }


  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    var state: RNG = rng
    val nums = List.fill(count) {
      val (i, s) = state.nextInt
      state = s
      i
    }

    (nums, state)
  }

  // 6.5
  def elegantDouble: Rand[Double] = {
    RNG.map(RNG.int)(i => i.toDouble / Int.MaxValue.toDouble)
  }

  // combine 2 rng actions into one
  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng: RNG => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }


  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fs.foldRight((List.empty[A], rng)) { case (rand: Rand[A], (acc: List[A], oldState: RNG)) =>
        val (i, newRng) =  rand(oldState)
        (i :: acc, newRng)
      }
    }
  }



  //6.8
  // generate a random A, then generate a random B based on that value
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng: RNG => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }


  def nonNegativeLessThan(n: Int): Rand[Int] = rng => {
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0) (mod, rng2)
      // TODO i think this should be rng2
    else nonNegativeLessThan(n)(rng)
  }


  def nonNegativeLessThan2(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan2(n)
    }
  }


  // 6.9
  def mapInTermsOfFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }


  def map2InTermsOfFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }


  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

// why is state not contravariant?
// is there any significant difference with run = (S => (S, A))?
// 6.10
case class State[S,+A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S, B] = {
    flatMap(a => unit(f(a)))
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => sb.map(b => f(a, b)))
  }

  // going with the analogy of flatMap = map andThen flatten,
  // run is like our "flatten"
  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State[S, B] { oldState =>
      val (a, newState) = this.run(oldState)
      f(a).run(newState)
    }
  }
}


object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

//  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
//    State[S, List[A]] { oldState =>
//      sas.foldLeft((unit[S, List[A]](List.empty[A]), oldState)) { case ((acc: State[Nothing, List[A]], os), elem: State[S, A]) =>
//        val (i, newState) = elem.run(os)
//        (i :: acc, newState)
//      }
//    }
//  }
}