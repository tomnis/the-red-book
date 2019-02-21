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
  }

  "foldRight" should "work" in {
    this.foldRight(List(1,2,3,4,5), 0)((a, acc) => acc + a) should be (15)
    this.foldRight(List(1,2,3,4,5), 0)( _ - _) should be (3)
  }
}
