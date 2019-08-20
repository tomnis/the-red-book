package exercises

import Process._

/**
  *
  * Created by tdm on 2019-08-19.
  */
class Chapter15Spec extends BaseSpec {

  "Process" should "work" in {
    val s = sum(Stream(1.0, 2.0, 3.0, 4.0)).toList

  }

}
