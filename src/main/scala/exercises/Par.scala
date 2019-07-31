package exercises

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, TimeUnit}

import scala.util.{Failure, Left, Right, Success, Try}

/**
  *
  * Created by tdm on 2019-05-19.
  */
object Par {


  // Thread and Runnable rely on side effects
  // Thread maps directly to OS threads

  // unit must begin evaluating its argument immediately in a separate (logical) thread

  // combine async computations without waiting for them to finish

  type Par[+A] = ExecutorService => AsyncFuture[A]

  /**
    * Creates a computation that immediately results in the value `a`.
    * Promotes a constant to a parallel computation.
    *
    * @param a
    * @tparam A
    * @return
    */
  def unit[A](a: A): Par[A] = {
    es => new AsyncFuture[A] {
      def apply(cb: A => Unit, onError: Throwable => Unit): Unit = cb(a)
    }
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
  def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] = {
    es => new AsyncFuture[C] {
      def apply(cb: C => Unit, onError: Throwable => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner: Actor[Either[A, B]] = Actor[Either[A,B]](es) {
          case Left(a: A) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a, b)), onError)
          }
          case Right(b: B) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a, b)), onError)
          }
        }
        p(es)(a => combiner ! Left(a), onError)
        p2(es)(b => combiner ! Right(b), onError)
      }
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
     es: ExecutorService => new AsyncFuture[A] {
      override def apply(cb: A => Unit, onError: Throwable => Unit): Unit = eval(es)(a(es)(cb, onError), onError)
    }
  }


  def eval(es: ExecutorService)(r: => Unit, onError: Throwable => Unit): Unit = {
    es.submit(
      new Callable[Unit] {
        override def call: Unit = try r catch { case t: Throwable => onError(t) }
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
  def run[A](es: ExecutorService)(p: Par[A]): A = {
    // mutable threadsafe reference for storing result
    val ref = new AtomicReference[Try[A]]
    // wait until method is called the specified number of times
    val latch = new CountDownLatch(1)
    p(es).apply(
      {a => ref.set(Success(a)); latch.countDown()},
      {t => ref.set(Failure(t)); latch.countDown()}
    )
    latch.await()
    println(s"done waiting for latch")
    ref.get.get
  }


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
    ps.foldRight(unit[List[A]](Nil)) { case (par, acc) =>
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

  def flatMap[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k: A = run(es)(p)
      val l: B = run(es)(choices(k))
      new AsyncFuture[B] {
        override private[exercises] def apply(k: B => Unit, onError: Throwable => Unit): Unit = k(l)
      }
    }
}


sealed trait AsyncFuture[+A] {
  private[exercises] def apply(k: A => Unit, onError: Throwable => Unit): Unit
}