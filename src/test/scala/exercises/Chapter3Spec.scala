package exercises

/**
  * Created by tomas.mccandless on 2019-02-21.
  */
class Chapter3Spec extends BaseSpec with Chapter3Like {

  "init" should "work" in {
    this.init(List(1,2,3,4,5)) should be (List(1,2,3,4))
  }


  "foldLeft" should "work" in {
    this.foldLeft(List(1,2,3,4,5), 0)((acc, a) => acc + a) should be (15)
    this.foldLeft(List(1,2,3,4,5), 0)((acc, a) => acc - a) should be (-15)
    this.foldLeftInTermsOfFoldRight(List(1,2,3,4,5), 0)((acc, a) => acc - a) should be (-15)
  }

  "foldRight" should "work" in {
    this.foldRight(List(1,2,3,4,5), 0)((a, acc) => acc + a) should be (15)
    this.foldRight(List(1,2,3,4,5), 0)( _ - _) should be (3)


    this.foldRight(List(1,2,3,4,5), 0)(_ - _) should be (List(1,2,3,4,5).foldRight(0)(_ - _))
  }




  "reverse" should "work" in {
    this.reverse(List(1,2,3)) should be (List(3,2,1))
  }

  "folddRight in terms of foldLeft" should "work" in {
    this.foldRightInTermsOfFoldLeft(List(1,2,3,4,5), 0)(_ - _) should be (List(1,2,3,4,5).foldRight(0)(_ - _))
    this.foldRightInTermsOfFoldLeft(List(1,2,3,4,5), 0)(_ + _) should be (List(1,2,3,4,5).foldRight(0)(_ + _))
  }



  "other stuff" should "work" in {
    this.flatten(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))) should be(List(1, 2, 3, 4, 5, 6, 7, 8, 9))

    this.increment(List(1, 2, 3)) should be(List(2, 3, 4))

    this.map(List(1, 2, 3, 4))(_ + 2) should be(List(3, 4, 5, 6))

    this.flatMap(List(1, 2, 3))(i => List(i, i)) should be(List(1, 1, 2, 2, 3, 3))

    this.add(List(1, 2, 3), List(4, 5, 6)) should be(List(5, 7, 9))
    this.zipWith(List(1, 2, 3), List(4, 5, 6))( (a, b) => a + b) should be(List(5, 7, 9))
  }
}
