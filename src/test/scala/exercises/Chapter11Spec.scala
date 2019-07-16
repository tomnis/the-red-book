package exercises

import Monads._

/**
  *
  * Created by tdm on 2019-07-15.
  */
class Chapter11Spec extends BaseSpec {

  "replicate" should "work" in {
    val m = Option(5)

    val n = optionMonad.replicateM(10, m)
    println(n)

  }



}
