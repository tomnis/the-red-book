package exercises

import scala.language.higherKinds

/**
  *
  * Created by tdm on 2019-07-16.
  */
trait Functor[F[_]] {

  /**
    * map(x)(a => a) = x
    *
    * @param fa
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def map[A, B](fa: F[A])(f: A => B): F[B]


  // unzip
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = {
    (this.map(fab)(_._1), this.map(fab)(_._2))
  }


  def codistribute[A, B](eab: Either[F[A], F[B]]): F[Either[A, B]] = eab match {
    case Left(fa) => map(fa)(Left.apply)
    case Right(fb) => map(fb)(Right.apply)
  }
}
