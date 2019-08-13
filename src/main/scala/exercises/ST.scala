package exercises

import scala.collection.mutable

/**
  * Local-effects
  *
  * Created by tdm on 2019-08-12.
  */
sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    override def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    override def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S,A](a: => A): ST[S, A] = {
    lazy val memo = a
    new ST[S,A] {
      override def run(s: S): (A, S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A = st.apply[Unit].run(())._1
}





// A, not S is the type of cell being mutated
// always tagged with the type S of the ST action that it lives in, it can never escape.
// possible to bypass with wildcard
sealed trait STRef[S,A] {
  protected var cell: A
  def read: ST[S,A] = ST(cell)
  def write(a: A): ST[S,Unit] = new ST[S,Unit] {
    override def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
}


object STRef {
  def apply[S,A](a: A): ST[S, STRef[S,A]] = ST(new STRef[S,A] {
    var cell = a })
}



//  Most importantly, we cannot run a program that tries to return a mutable reference.
trait RunnableST[A] {
  def apply[S]: ST[S,A]
}




sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {

  protected def value: Array[A]

  def size: ST[S, Int] = ST(this.value.length)

  def write(i: Int, a: A): ST[S, Unit] =  new ST[S,Unit] {
    override def run(s: S): (Unit, S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S,A] = ST(value(i))

  def freeze: ST[S,List[A]] = ST(value.toList)

  def fill(xs: Map[Int,A]): ST[S,Unit] = {
    xs.foldRight(ST[S,Unit](())) {
      case ((k, v), st: ST[S, Unit]) => st flatMap (_ => write(k, v))
    }
  }

  def swap(i: Int, j: Int): ST[S,Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()
}


object STArray {
  def apply[S, A:Manifest](sz: Int, v: A): ST[S, STArray[S,A]] = ST {
    new STArray[S,A] {
      override lazy val value: Array[A] = Array.fill(sz)(v)
    }
  }
}

object Quicksort {
  def noop[S]: ST[S, Unit] = ST[S,Unit](())


  def partition[S](a: STArray[S,Int], l: Int, r: Int, pivot: Int): ST[S,Int] = for {
    vp <- a.read(pivot)
    _ <- a.swap(pivot, r)
    j <- STRef(l)
    _ <- (l until r).foldLeft(noop[S])((s, i) => for {
      _ <- s
      vi <- a.read(i)
      _  <- if (vi < vp) (for {
        vj <- j.read
        _  <- a.swap(i, vj)
        _  <- j.write(vj + 1)
      } yield ()) else noop[S]
    } yield ())
    x <- j.read
    _ <- a.swap(x, r)
  } yield x

  def qs[S](a: STArray[S,Int], l: Int, r: Int): ST[S,Unit] = if (l < r) for {
    pi <- partition(a, l, r, l + (r - l) / 2)
    _ <- qs(a, l, pi - 1)
    _ <- qs(a, pi + 1, r)
  } yield () else noop[S]


}


sealed abstract class STHashMap[S, K, V](implicit m1: Manifest[K], m2: Manifest[V]) { self =>

  protected def storage: mutable.HashMap[K, V]

  def size: ST[S, Int] = ST(self.storage.size)

  def write(key: K, value: V): ST[S, Unit] =  new ST[S,Unit] {
    override def run(s: S): (Unit, S) = {
      self.storage += (key -> value)
      ((), s)
    }
  }

  def read(key: K): ST[S, Option[V]] = ST(self.storage get key)

  def freeze: ST[S, Map[K, V]] = ST(self.storage.toMap)
}
