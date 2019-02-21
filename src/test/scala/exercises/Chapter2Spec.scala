package exercises

class Chapter2Spec extends BaseSpec with Chapter2 {

  "fib" should "work" in {
    this.fib(0) should be (0)
    this.fib(1) should be (1)
    this.fib(5) should be (5)
    this.fib(6) should be (8)
  }

  "isSorted" should "work" in {
    val asc: Array[Int] = Array(1,2,3,3,4,5)
    val desc: Array[Int] = asc.reverse
    val ascOrder: (Int, Int) => Boolean = (a, b) => a <= b
    val descOrder: (Int, Int) => Boolean = (a, b) => a >= b

    this.isSorted(asc, ascOrder) should be (true)
    this.isSorted(asc, descOrder) should be (false)
    this.isSorted(desc, ascOrder) should be (false)
    this.isSorted(desc, descOrder) should be (true)
  }
}
