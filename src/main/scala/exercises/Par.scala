package exercises

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent._

/**
  *
  * Created by tdm on 2019-05-19.
  */
object Par {


  // Thread and Runnable rely on side effects
  // Thread maps directly to OS threads

  // unit must begin evaluating its argument immediately in a separate (logical) thread

  // combine async computations without waiting for them to finish

  type Par[A] = ExecutorService => Future[A]

  /**
    * Creates a computation that immediately results in the value `a`.
    * Promotes a constant to a parallel computation.
    *
    * @param a
    * @tparam A
    * @return
    */
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }


  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))

  /**
    * Combines the results of two parallel computations.
    *
    * @param para
    * @param parb
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    es: ExecutorService => {
      val af = a(es)
      val bf = b(es)
      UnitFuture (f(af.get, bf.get))
    }
  }


//  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
//    val x: ((A, B) => C) => Par[C] = map2(a, b)
//  }


  /**
    * Marks a computation for concurrent evaluation by run.
    * @param a
    * @tparam A
    * @return
    */
  def fork[A](a: => Par[A]): Par[A] = {
    es: ExecutorService => es.submit(
      new Callable[A] {
        override def call: A = a(es).get
      }
    )
  }


  /**
    * Wraps expression `a` for concurrent evaluation.
    * @param a
    * @tparam A
    * @return
    */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))



  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }



  /**
    * Fully evaluates a Par
    * @param a
    * @tparam A
    * @return
    */
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)


  def sum(ints: List[Int]): Par[Int] = {
    if (ints.length <= 1) Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
  }

  // we’ve carved out a little universe for ourselves.
  // We now get to discover what ideas are expressible in this universe

  // 7.4
  def asyncF[A,B](f: A => B): A => Par[B] = {
    a: A => {
      lazyUnit(f(a))
    }
  }


  // 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit[List[A]](Nil)) { case (par: Par[A], acc: List[Par[A]]) =>
      map2(par, acc)(_ :: _)
    }
  }

  // 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val maybes: Par[List[Option[A]]] = parMap(as)(a => Option(a).filter(f))
    map(maybes)(_.flatten)
  }

  // 7.7
  // map(y)(id) == y
  //

  // map(map(y)(g))(f) == map(y)(f compose g)

  def delay[A](fa: => Par[A]): Par[A] = (es: ExecutorService) => fa(es)


  // 7.11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es => {
      val ind: Int = run(es)(n).get
      run(es)(choices(ind))
    }
  }


  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    val ind: Par[Int] = map(cond)(c => if (c) 0 else 1)
    choiceN(ind)(List(t, f))
  }

  // 7.12
  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = {
    es => {
      val k: K = run(es)(key).get
      run(es)(choices(k))
    }
  }


  // 7.13
  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    es => {
      val a: A = run(es)(pa).get
      run(es)(choices(a))
    }
  }

  def choiceChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    chooser[Boolean, A](cond)((b: Boolean) => if (b) t else f)
  }

  def choiceNChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    chooser[Int, A](n)((ind: Int) => choices(ind))
  }


  // 7.14
  def join[A](a: Par[Par[A]]): Par[A] = {
    es => {
      run(es)(run(es)(a).get())
    }
  }

  // flatmap using join
  // map and then flatten
  def flatMapJoin[A,B](a: Par[A])(f: A => Par[B]): Par[B] = {
    join(map(a)(f))
  }

  // join using flatMap
  def joinFlatMap[A](a: Par[Par[A]]): Par[A] = {
    flatMapJoin[Par[A], A](a)(x => x)
  }
}


//sealed trait Future[A] {
//  private[exercises] def apply(k: A => Unit): Unit
//}