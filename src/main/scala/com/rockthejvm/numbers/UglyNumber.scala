package com.rockthejvm.numbers

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object UglyNumber extends App {

  // ugly = only the factors 2, 3, 5
  // 1 is ugly
  // assume positive inputs
  // examples: 6, 25, 100
  // not ugly: 14, 39

  // my implementation
//  def uglyNumber(number: Int): Boolean = {
//    val uglyFactors = List(2,3,5)
//
//    @tailrec
//    def decomposeTailrec(current: Int, acc: List[Int]): List[Int] = {
//      if (current > Math.sqrt(number)) acc.reverse ++ acc.map(number / _)
//      else if (number % current == 0) decomposeTailrec(current + 1, current :: acc)
//      else decomposeTailrec(current + 1, acc)
//    }
//
//    val factors = decomposeTailrec(2, List())
//    val nonUglyFactors = factors.filterNot(_ == 2).filterNot(_ == 3).filterNot(_ == 5)
//
//    if (uglyFactors.contains(number)) true
//    else factors.nonEmpty && nonUglyFactors.isEmpty
//  }

  // daniel's impl
  def uglyNumber_v2(number: Int): Boolean = {
    if (number == 1) true
    else if (number % 2 == 0) uglyNumber_v2(number / 2)
    else if (number % 3 == 0) uglyNumber_v2(number / 3)
    else if (number % 5 == 0) uglyNumber_v2(number / 5)
    else false
  }

  // determine the nth ugly number
  // 1 is the first ugly number

  // my impl
  def nthUgly(index: Int): Int = {
    @tailrec
    def nthUglyTailrec(current: Int, lastUgly: Int, uglyFound: Int): Int = {
      if (uglyFound == index) lastUgly
      else if (uglyNumber_v2(current)) nthUglyTailrec(current + 1, current, uglyFound + 1)
      else nthUglyTailrec(current + 1, lastUgly, uglyFound)
    }

    nthUglyTailrec(1, 0, 0)
  }

  // daniel's impl
  def nthUgly_v2(n: Int): Int = {
    /*
    1 -> 2 -> 3 -> 4 -> 5 -> 6

    pop smallest value from all three queues
    [2] -> [4]    -> [4,6]     -> [6,8]        -> [6,8,10]      -> [8,10,12]
    [3] -> [3,6]  -> [6,9]     -> [6,9,12]     -> [6,9,12,15]   -> [9,12,15,18]
    [5] -> [5,10] -> [5,10,15] -> [5,10,15,20] -> [10,15,20,25] -> [10,15,20,25,30]
     */

    def min3(a: Int, b: Int, c: Int): Int = {
      if (a <= b){
        if (a <= c) a
        else c
      } else if (b <= c) b
      else c
    }

    @tailrec
    def nthUglyTailrec(index: Int, q2: Queue[Int], q3: Queue[Int], q5: Queue[Int]): Int = {
      val min = min3(q2.head, q3.head, q5.head)

      if (index == n) min
      else {
        val newQ2 = (if (min == q2.head) q2.tail else q2).enqueue(min * 2)
        val newQ3 = (if (min == q3.head) q3.tail else q3).enqueue(min * 3)
        val newQ5 = (if (min == q5.head) q5.tail else q5).enqueue(min * 5)

        nthUglyTailrec(index + 1, newQ2, newQ3, newQ5)
      }
    }

    if (n == 1) 1
    else nthUglyTailrec(2, Queue(2), Queue(3), Queue(5))

  }

  // mine is bugged somehow
//  println("my tests")
//  println(uglyNumber(10))
//  println(uglyNumber(11))
//  println(uglyNumber(256))
//  println(uglyNumber(1))
//  println(uglyNumber(2))
//  println(uglyNumber(3))
//  println(uglyNumber(5))
//  println(uglyNumber(1200))

  println("daniel test")
  println(uglyNumber_v2(10))
  println(uglyNumber_v2(11))
  println(uglyNumber_v2(256))
  println(uglyNumber_v2(1))
  println(uglyNumber_v2(2))
  println(uglyNumber_v2(3))
  println(uglyNumber_v2(5))
  println(uglyNumber_v2(1200))

  println("\nnth ugly - mine")
  println(nthUgly(100))
  println((1 to 25).map(nthUgly))

  println("\nnth ugly - Daniel")
  println(nthUgly_v2(100))
  println((1 to 25).map(nthUgly_v2))

}
