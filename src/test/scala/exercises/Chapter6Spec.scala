package exercises

import exercises.RNG.{Rand, Simple}

/**
  * Created by tomas.mccandless on 2019-03-18.
  */
class Chapter6Spec extends BaseSpec {


  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, s) = rng.nextInt
    i match {
      case Int.MinValue => (Int.MaxValue, s)
      case other => (other, s)
    }
  }


  "nonnegative" should "work" in {

  }




  def double(rng: RNG): (Double, RNG) = {
    val (i, s) = rng.nextInt
    (i / Int.MaxValue.toDouble, s)
  }



  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, s) = rng.nextInt
    val (d,s2) = double(s)
    ((i, d), s2)
  }


  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d,s) = double(rng)
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


  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fs.foldRight((List.empty[A], rng)) { case (rand: Rand[A], (acc: List[A], oldState: RNG)) =>
        val (i, newRng) =  rand(oldState)
        (i :: acc, newRng)
      }
    }
  }


  "sequence" should "work" in {
    val s: RNG = Simple(42)
    val r: Rand[Int] = s => s.nextInt
    val q: Rand[String] = RNG.map(r)(_.toString)
    val rs = List.fill(4)(q)

    val seq = sequence(rs)
    println(rs)
  }
}