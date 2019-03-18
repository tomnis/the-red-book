package exercises

/**
  *
  * Created by tdm on 2019-03-04.
  */
class Chapter4Spec extends BaseSpec {

  "FpOption" should "work" in {

    FpOption(2).map(_ + 1) should be (FpSome(3))

    FpOption(2).orElse(FpOption(3)) should be (FpSome(2))
    FpNone.orElse(FpOption(3)) should be (FpSome(3))


    FpOption(2).filter(_ > 0) should be (FpSome(2))
    FpOption(2).filter(_ < 0) should be (FpNone)
  }

  "variance" should "work" in {

    def mean(xs: Seq[Double]): FpOption[Double] =
      if (xs.isEmpty) FpNone
      else FpSome(xs.sum / xs.length)

    def variance(xs: Seq[Double]): FpOption[Double] = {

      mean(xs).flatMap { m =>
        mean(xs.map { x => math.pow(x - m, 2) })
      }

      for {
        avg <- mean(xs)
        vari <- mean(xs.map(x => math.pow(x - avg, 2)))
      } yield vari
    }
  }




  // 4.3
  def bimap[A,B,C](maybeA: FpOption[A], maybeB: FpOption[B])(f: (A, B) => C): FpOption[C] = {
    maybeA.flatMap { a =>
      maybeB.map(b => f(a, b))
    }

    for {
      a <- maybeA
      b <- maybeB
    } yield f(a, b)
  }

  "bimap" should "work" in {


    bimap(FpOption(1), FpOption(2))(_ + _) should be (FpOption(3))
    bimap(FpNone, FpOption(2))((a: Int, b) => a + b) should be (FpNone)
    bimap(FpOption(1), FpNone)(_ + _) should be (FpNone)
  }



  // 4.4
  def sequence[A](as: List[FpOption[A]]): FpOption[List[A]] = {
    val reversed: FpOption[List[A]] = as.foldLeft(FpOption(List.empty[A])) { case (maybeAcc: FpOption[List[A]], maybeA: FpOption[A]) =>
      bimap(maybeA, maybeAcc)(_ :: _)
    }
    reversed.map(_.reverse)
  }

  // TODO contrast with slick dbio sequence
  "sequence" should "work" in {
    sequence(List(FpOption(1), FpOption(2), FpOption(3))) should be (FpOption(List(1,2,3)))
    sequence(List(FpOption(1), FpNone, FpOption(3))) should be (FpNone)
  }




  // 4.5
  def traverse[A, B](as: List[A])(f: A => FpOption[B]): FpOption[List[B]] = {
    sequence(as.map(f))

    as.foldRight(FpOption(List.empty[B])) { case (a: A, maybeAcc: FpOption[List[B]]) =>
      bimap(f(a), maybeAcc)(_ :: _)
    }
  }

  "traverse" should "work" in {


  }


  // 4.7


  // 4.8
  // def mkPerson(name: String, age: Int): Either[String, Person] =
  // use a nonempty seq?
  // def mkPerson(name: String, age: Int): Either[Seq[String], Person] =
  // mkPerson("", -1)
}
