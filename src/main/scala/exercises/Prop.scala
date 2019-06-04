package exercises

import Prop._

/**
  *
  * Created by tdm on 2019-06-02.
  */
sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified = false
}

case class Falsified(failure: FailedCase,
                     successes: SuccessCount) extends Result {
  override def isFalsified = true
}

case object Proved extends Result {
  override def isFalsified = false
}

//trait Prop {
  // 8.3
//  def check: Boolean
//  def &&(p: Prop): Prop = {
//    val res = this.check
//    new Prop {
//      override def check = res && p.check
//    }
//  }
//  def check: Either[(FailedCase, SuccessCount), SuccessCount]
//}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (maxSize, testCases, rng) => {
      val result: Result = this.run(maxSize, testCases, rng)
      result match {
        case Passed | Proved => p.run(maxSize, testCases, rng)
        case failed => failed
      }
    }
  }


  def ||(p: Prop): Prop = Prop {
    (maxSize, testCases, rng) => {
      val result: Result = this.run(maxSize, testCases, rng)
      result match {
        case Falsified(_, _) => p.run(maxSize, testCases, rng)
        case worked => worked
      }
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  }
  private def buildMsg[A](a: A, exception: Exception): String = {
    s"test case $a resuled in exception $exception"
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.filter(_.isFalsified).headOption.getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop { (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }


  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }
  }
}

