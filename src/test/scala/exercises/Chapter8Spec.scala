package exercises

import SGen._
import Prop._

/**
  *
  * Created by tdm on 2019-06-02.
  */
class Chapter8Spec extends BaseSpec {
  // 8.1
  // tail.sum should be l.sum - l.head

  // 8.2 max
  // forall x in l, x <= l.max
  //

  "tests" should "work" in {
    val smallInt: Gen[Int] = Gen.choose(-10,10)
    val maxProp: Prop = forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    Prop.run(maxProp)
  }

  "par" should "work" in {
    val p2 = Prop.check {
      val p = Par.map(Par.unit(1))(_ + 1) val p2 = Par.unit(2)
      p(ES).get == p2(ES).get
    }
  }

}
