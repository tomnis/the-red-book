package exercises

import scala.annotation.tailrec

trait Chapter2 {

  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, fibn2: Int, fibn1: Int): Int = {
      if (n <= 0) fibn2
//      else if (n <= 1) fibn1
      else go(n - 1, fibn1, fibn1 + fibn2)
    }

    go(n, 0, 1)
  }


  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    val pairs: Iterator[Array[A]] = as.sliding(2)
    pairs.forall(pair => ordered(pair.head, pair.tail.head))
//    pairs.foldLeft(true)((acc, pair) => acc && ordered(pair.head, pair.tail.head))
  }
  

  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a: A => (b: B) => f(a, b)
  }


  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }


  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }

//  def foo(a: Int)(b: Int)
}
