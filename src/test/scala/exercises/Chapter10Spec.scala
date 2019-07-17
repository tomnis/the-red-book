package exercises

import exercises.Monoids.Stub

/**
  *
  * Created by tdm on 2019-07-03.
  */
class Chapter10Spec extends BaseSpec {

  "option monoid" should "work" in {
    val om = Monoids.optionMonoid[Int]
    // associative
    om.combine(Option(1), om.combine(Option(2), Option(3))) should be (om.combine(om.combine(Option(1), Option(2)), Option(3)))
  }


  "split" should "work" in {
    Monoids.split(IndexedSeq(1)) should be ((IndexedSeq.empty, IndexedSeq(1)))
    Monoids.split(IndexedSeq(1,2)) should be ((IndexedSeq(1), IndexedSeq(2)))
    Monoids.split(IndexedSeq(1,2,3)) should be ((IndexedSeq(1), IndexedSeq(2,3)))
    Monoids.split(IndexedSeq(1,2,3,4)) should be ((IndexedSeq(1,2), IndexedSeq(3,4)))
    Monoids.split(IndexedSeq(1,2,3,4,5)) should be ((IndexedSeq(1,2), IndexedSeq(3,4,5)))
  }

  "isOrdered" should "work" in {
    implicit val ordering: Ordering[Int] = (x: Int, y: Int) => y - x

    Monoids.isOrdered(IndexedSeq(1,2,3,4))(ordering) should be (true)
    Monoids.isOrdered(IndexedSeq(4,3,2,1))(ordering) should be (false)
    Monoids.isOrdered(IndexedSeq(1,2,4,3))(ordering) should be (false)

  }

  "count" should "work" in {
    Monoids.countWords("a b c") should be (3)
  }

  "wc" should "?" in {
    Monoids.wcMonoid.combine(Stub("a"), Stub("b"))
  }
}
