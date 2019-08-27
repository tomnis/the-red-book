package exercises

import Process._

/**
  *
  * Created by tdm on 2019-08-19.
  */
class Chapter15Spec extends BaseSpec {

  "Process" should "work" in {
    sum(Stream(1.0, 2.0, 3.0, 4.0)).toList should be (List(1.0, 3.0, 6.0, 10.0))

    count(Stream("a", "b", "c", "d")).toList should be (List(1, 2, 3, 4))

    mean(Stream(1.0, 2.0, 3.0, 4.0)).toList should be (List(1.0, 1.5, 2.0, 2.5))
    zipWithIndex(Stream("a", "b", "c", "d")).toList should be (List(("a", 0), ("b", 1), ("c", 2), ("d", 3)))
  }

}
