package com.rockthejvm.numbers

import scala.annotation.tailrec
import scala.util.Random

object LargestNumber extends App {

  /*
  Given a list of non-negative integers, arrange them such that they form the largest number possible.
  The result might be huge, so return a string

  List(10, 2) => "210"
  List(3, 30, 5, 9, 34) => "9533430" -> [3,5,9,30,34]
   */

  // my solution - bugged, I evaluated the string incorrectly
  def largestNumber(numbers: List[Int]): String = {
//    @tailrec
//    def selectOrder(order: Int, list: List[Int], lessThan: List[Int], greaterThan: List[Int]): (List[Int], List[Int]) = {
//      if (list.isEmpty) (lessThan, greaterThan)
//      else if (list.head < order) selectOrder(order, list.tail, list.head :: lessThan, greaterThan)
//      else selectOrder(order, list.tail, lessThan, list.head :: greaterThan)
//    }
//
//    @tailrec
//    def largestNumberTailrec(remaining: List[Int], limit: Int, accumulator: List[Int]): List[Int] = {
//      if (remaining.isEmpty) accumulator.reverse
//      else if (remaining.head / limit != 0) largestNumberTailrec(remaining, 10 * limit, accumulator)
//      else {
//        val (lessThan, rem) = selectOrder(limit, remaining, List(), List())
//        largestNumberTailrec(rem.sorted, limit, lessThan.reverse ++ accumulator)
//      }
//    }
//    largestNumberTailrec(numbers.sorted, 10, List()).mkString("")

    // daniel's solution
    implicit val newOrdering: Ordering[Int] = Ordering.fromLessThan { (a, b) =>
      // concatenate a with b => ab
      // concatenate b with a => ba
      // comparison: a comes before b if ab >= ba
      val aStr = a.toString
      val bStr = b.toString

      (aStr + bStr).compareTo(bStr + aStr) >= 0
    }
    /*
    needed properties for a valid (aka sound) ordering
    - reflexive: a <= a
    - anti-symmetrical: if a <= b AND b <= a THEN a == b (This is not the case for newOrdering)
    - transitivity: if a <= b AND b <= c THEN a <= c
     */

    val largest = numbers.sorted.mkString("")
    if (numbers.isEmpty || largest.charAt(0) == '0') "0"
    else largest
  }

  val random = new Random(System.currentTimeMillis())
  val randList = (1 to 10).map(_ => random.between(1, 1000)).toList
  println(randList)

  println(largestNumber(List(3, 30, 5, 9, 34)))
  println(largestNumber(List(10, 2)))
  println(largestNumber(List(2020, 20, 1010, 10, 2, 22)))
  println(largestNumber(List(1)))
  println(largestNumber(List()))
  println(largestNumber(List(0, 0, 0)))
  println(largestNumber(randList))
}
