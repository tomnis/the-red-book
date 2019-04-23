package exercises

import exercises.RNG.{Rand, Simple}
import RNG._

/**
  * Created by tomas.mccandless on 2019-03-18.
  */
class Chapter6Spec extends BaseSpec {


  "nonnegative" should "work" in {

  }



  "map2" should "work" in {
    val r1: Rand[Int] = RNG.int
    val r2: Rand[Double] = RNG.elegantDouble

    val intAndDouble: Rand[(Int, Double)] = RNG.map2(r1, r2)((a, b) => (a + 1, b + 2))
  }



  "sequence" should "work" in {
    val s: RNG = Simple(42)
    val r: Rand[Int] = s => s.nextInt
    val q: Rand[String] = RNG.map(r)(_.toString)
    val rs = List.fill(4)(q)

    val seq = sequence(rs)
    println(rs)
  }

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    val unlocked: Boolean = !this.locked
  }

  def runInput(m: Machine, input: Input): Machine = {
    (m, input) match {
      // out of candy => ignore all inputs
      case (m, _) if m.candies < 1 => m
      // inserting coin into locked machine => machine is unlocked, increment coins
      case (m, Coin) if m.locked => m.copy(locked = false, coins = m.coins + 1)
      // turn knob on unlocked machine => dispense candy, machine becomes locked
      case (m, Turn) if m.unlocked => m.copy(locked = true, candies = m.candies - 1)
      // turning knob on locked machine => nothing
      // inserting coin in unlocked machine => nothing
      case _ => m
    }
  }

  /**
    *
    * @param inputs
    * @return number of coins and candies left in the machine
    */
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State[Machine, (Int, Int)] { oldMachine =>
      val newMachine: Machine = inputs.foldLeft(oldMachine) {
        case (lastMachine, input) => runInput(lastMachine, input)
      }
      ((newMachine.candies, newMachine.coins), newMachine)
    }
  }

  // 6.11
  "candy dispenser" should "work" in {
    // input types: coin, turn knob
    // states: locked or unlocked
    // tracks how many candies are left, how many coins it contains
    val m1: Machine = Machine(true, 1, 0)
    val expected: Machine = Machine(true, 0, 1)

    // putting a coin in a locked machine should unlock it
    simulateMachine(List(Coin)).run(m1)._2 should be (Machine(false, 1, 1))
    // turning an unlocked machine should lock it and release candy
    simulateMachine(List(Coin, Turn)).run(m1)._2 should be (expected)
    // further inputs to an empty machine should be ignored
    simulateMachine(List(Coin, Turn, Coin)).run(m1)._2 should be (expected)
    simulateMachine(List(Coin, Turn, Turn)).run(m1)._2 should be (expected)

    // unlocked machine should ignore a coin
    val m2: Machine = Machine(false, 1, 0)
    simulateMachine(List(Coin)).run(m2)._2 should be (m2)

    // locked machine should ignore a turn
    val m3: Machine = Machine(true, 1, 0)
    simulateMachine(List(Turn)).run(m3)._2 should be (m3)
  }
}
