package exercises

import Stream._

/**
  *
  * Created by tdm on 2019-03-11.
  */
class Chapter5Spec extends BaseSpec {
  // Non-strictness is a property of a function.
  // To say a function is non-strict just means that the function may choose not to evaluate one or more of its arguments.
  // In contrast, a strict function always evaluates its arguments.

  // Due to technical limitations, these are thunks that must be explicitly forced, rather than by-name parameters.


  "stream" should "work" in {

    val s: Stream[Int] = Stream(1,2,3)
    s.toList should be (List(1,2,3))

    s.drop(1).toList should be (Stream(2,3).toList)
    s.drop(2).toList should be (Stream(3).toList)

    s.take(1).toList should be (List(1))
    s.take(2).toList should be (List(1,2))

    val s2: Stream[Int] = Stream(2,4,6,8,9)
    s2.takeWhile(a => a % 2 == 0).toList should be (List(2,4,6,8))

    var counter = 0
    Stream(2,5,6,7,8).forAll { a =>
      counter += 1
      a % 2 == 0
    } should be (false)
    counter should be (2)

    Stream(2,4,6,8).forAll(a => a % 2 == 0) should be (true)


    Stream(2,3,4).headOption should be (Some(2))

    Stream(2,3,4).map(_ + 1).toList should be (List(3,4,5))

    Stream(2,3,4,5,6).filter(a => a % 2 == 0).toList should be (List(2,4,6))

    Stream(2,3,4,5).flatMap(a => Stream(a, a)).toList should be (List(2,2,3,3,4,4,5,5))

//    val ones: Stream[Int] = Stream.cons(1, ones)
//    ones.toList
    Stream.fibs.take(5).toList should be (List(0,1,1,2,3))

    Stream.onesUnfold.take(5).toList should be (List(1,1,1,1,1))

    Stream.fromUnfold(3).take(4).toList should be (List(3,4,5,6))
  }

  // filter and map transformations are inter- leaved


}
