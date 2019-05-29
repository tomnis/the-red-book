package exercises

import java.util.concurrent.{Executor, ExecutorService, Executors}

import Par._

/**
  *
  * Created by tdm on 2019-05-19.
  */
class Chapter7Spec extends BaseSpec {

//  val pool: ExecutorService = Executors.newFixedThreadPool(4)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = Par.run(e)(p) == Par.run(e)(p2)

  "async" should "fail" in {
    val p: Par[Int] = map(unit(2))(a => a / 0)
    Par.run(Executors.newFixedThreadPool(1))(p)
  }

  "fork" should "deadlock" in {
//    val a = lazyUnit(42 + 1)
//    val S = Executors.newFixedThreadPool(1)
//    println(equal(S)(a, fork(a)))
    val a = lazyUnit(42 + 1)
    val b = Par.map(a)(_ + 1)
    val S = Executors.newFixedThreadPool(2)
    println(equal(S)(fork(b), fork(a)))
  }

  "actors" should "work" in {
    val p : Par[List[Double]] = Par.parMap(List.range(1, 10))(math.sqrt(_))
    val x: Seq[Double] = Par.run(Executors.newFixedThreadPool(2))(p)
    println(x take 3)
  }


  def numWords(paragraphs: List[String]): Par[Int] = {
    val parParagraphs: Par[List[Int]] = parMap(paragraphs) {
      paragraph => paragraph.split(" ").length
    }

    Par.map(parParagraphs)(_.sum)
  }


  def parReduce[A, B](as: List[A])(f: A => B, g: (B, B) => B): Par[B] = {
    val transformedAs: Par[List[B]] = parMap(as)(f)
    Par.map(transformedAs)(_.reduce(g))
  }
}
