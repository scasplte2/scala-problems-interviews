package com.rockthejvm.numbers

import com.rockthejvm.lists.{RList, RNil}

import scala.annotation.tailrec

object NumberOps {
  implicit class RRichInt(n: Int) {
    /*
    isPrime(11) = ipt(2)
    = 11 % 2 != 0
    = ipt(3) = 11 % 3 != 0
    = ipt(4)
    = true

    Complexity: O(sqrt(N))
     */
    def isPrime: Boolean = {
      @tailrec
      def isPrimeTailrec(currentDivisor: Int): Boolean = {
        if (currentDivisor > Math.sqrt(Math.abs(n))) true
        else n % currentDivisor != 0 && isPrimeTailrec(currentDivisor + 1)
      }

      if (n == 0 || n == 1) false
      else isPrimeTailrec(2)
    }

    // decompose n into constituent prime divisors
    def decompose(n: Int): RList[Int] = {
      @tailrec
      def decomposeTailrec(current: Int, accumulator: RList[Int]): RList[Int] = {
        assert(n >= 0) //error on negative numbers
        if (current > Math.sqrt(n)) accumulator.reverse ++ accumulator.map(n / _)
        else if (n % current == 0) decomposeTailrec(current + 1, current :: accumulator)
        else decomposeTailrec(current + 1, accumulator)
      }

      // Daniel's solution - less complex due to not having to reverse
      // Complexity: max - O(sqrt(N)), min O(log(N))
      /*
      decompose(1024) = dtr(1024, 2, [])
      = dtr(512, 2, [2])
      = dtr(256, 2, [2,2]) -> seems to be a bug
       */
      @tailrec
      def decomposeTailrec_v2(remaining: Int, current: Int, accumulator: RList[Int]): RList[Int] = {
        if (current > Math.sqrt(remaining)) remaining :: accumulator
        else if (remaining % current == 0) decomposeTailrec_v2(remaining / current, current, current :: accumulator)
        else decomposeTailrec_v2(remaining, current + 1, accumulator)
      }

      decomposeTailrec(1, RNil)
      //decomposeTailrec_v2(n, 2, RNil)
    }
  }
}
object NumberProblems extends App {
  import NumberOps._ // bring implicit class into scope

  def testIsPrimse(): Unit = {
    println("\n isPrime test")
    println(2.isPrime)
    println(15.isPrime)
    println(2003.isPrime)
    println(2731189.isPrime)
    println(254266841.isPrime)
    println(1.isPrime)
    println(0.isPrime)
    println((-2003).isPrime)
  }

//  testIsPrimse()

//  def testDecompose(): Unit = {
//    println("\n decompose test")
//    println(decompose(10))
//    //  println(decompose(-10))
//    //  println(decompose(0))
//    println(decompose(1024))
//    println(decompose(1111))
//    println(decompose(11))
//    println(decompose(15))
//    println(decompose(1))
//    println(decompose(2))
//    println(decompose(15))
//    println(decompose(2003))
//    println(decompose(2731189))
//    println(decompose(517935871))
//    println(decompose(1))
//    println(decompose(0))
//  }

  //testDecompose()



}
