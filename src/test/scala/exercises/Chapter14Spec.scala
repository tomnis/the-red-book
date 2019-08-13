package exercises

/**
  *
  * Created by tdm on 2019-08-12.
  */
class Chapter14Spec extends BaseSpec {
  "st" should "work" in {
    val p = new RunnableST[(Int, Int)] {
      override def apply[S]: ST[S, (Int, Int)] = for {
        r1 <- STRef(1)
        r2 <- STRef(1)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(y + 1)
        _ <- r2.write(x + 1)
        a <- r1.read
        b <- r2.read
      } yield (a, b)
    }

//    val s = new RunnableST[STRef[Nothing,Int]] {
//      override def apply[S] = STRef(1)
//    }
  }
}
