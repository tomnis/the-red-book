package exercises

/**
  *
  * Created by tdm on 2019-06-03.
  */
case class Gen[+A](sample: State[RNG,A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(this.sample.flatMap(a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap { n =>
      Gen(State.sequence(List.fill(n)(this.sample)))
    }
  }



  // 8.10
  def unsized: SGen[A] = SGen[A](_ => this)
}


object Gen {

  // 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {

    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
  }

  // 8.5
  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  def boolean: Gen[Boolean] = {
    Gen(State(RNG.nonNegativeLessThan(2)).map(n => n % 2 == 0))
  }

  // 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    this.weighted((g1, 0.5), (g2, 0.5))
  }

  // 8.8
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    Gen(State(RNG.double).flatMap { d =>
      val inUnitInterval: Double = scala.math.abs(d) / Double.MaxValue
      if (inUnitInterval <= g1._2) g1._1.sample else g2._1.sample
    })
  }




}

case class SGen[+A](forSize: Int => Gen[A]) {

  // 8.11
  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen { size =>
    this.forSize(size).flatMap(a => f(a).forSize(size))
  }

}

object SGen {

  // 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(size => g.listOfN(Gen.unit(size)))

  // 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] = {
    SGen(size => g.listOfN(Gen.unit(size max 1)))
  }
}
