package com.rockthejvm.strings

import scala.annotation.tailrec

object MultiplyStrings extends App {

  // multiply two numbers represented as strings of arbitrary length
  // don't use conversion libraries directly but can use as intermediate

  // my impl
  // doesn't work for very large numbers, I believe due to the repr of the power in the Map
  def multiplyStrings(a: String, b: String): String = {
    val aList = a.map(_ - '0').toList.reverse
    val bList = b.map(_ - '0').toList.reverse


    @tailrec
    def multiplyDigit(num: List[Int], digit: Int, magnitude: Long, acc: Map[Long, Int]): Map[Long, Int] = {
      if (num.isEmpty) acc
      else {
        val product = num.head * digit
        val currentSum = acc.getOrElse(magnitude, 0)
        val newAcc = acc + (magnitude -> (currentSum + product))
        multiplyDigit(num.tail, digit, 10L * magnitude, newAcc)
      }
    }

    @tailrec
    def multiplyAll(num: List[Int], magnitude: Long, acc: List[Map[Long, Int]]): List[Map[Long, Int]] = {
      if (num.isEmpty) acc
      else {
        val products = multiplyDigit(aList, num.head, magnitude, Map())
        multiplyAll(num.tail, 10L * magnitude, products :: acc)
      }
    }

    @tailrec
    def carry(num: List[Int], carryover: Int, acc: List[Int]): List[Int] = {
      if (num.isEmpty) acc
      else if ((num.head + carryover) >= 10) {
        val newValue = (num.head + carryover) % 10
        val newCarry = (num.head + carryover) / 10
        val newAcc = newValue :: acc
        carry(num.tail, newCarry, newAcc)
      } else {
        carry(num.tail, 0, (num.head + carryover) :: acc)
      }
    }

    val listProducts = multiplyAll(bList, 1, List())
    val powers = listProducts.foldLeft(Set[Long]()){ case (acc, map) => acc ++ map.keySet }.toList.sorted
    val powerSums = powers.map(p => listProducts.foldLeft(0)((acc,map) => acc + map.getOrElse(p,0)))
    carry(powerSums, 0, List()).mkString
  }

  // Daniel's impl
  def multiplyStrings_v2(a: String, b: String): String = {

    def multiplyByDigit(number: List[Int], digit: Int): List[Int] = {
      @tailrec
      def multiplyByDigitTailrec(remaining: List[Int], carry: Int, acc: List[Int]): List[Int] = {
        if (remaining.isEmpty) {
          if (carry == 0) acc.reverse
          else (carry :: acc).reverse
        }
        else {
          val newDigit = remaining.head
          val newProduct = newDigit * digit + carry
          multiplyByDigitTailrec(remaining.tail, newProduct / 10, (newProduct % 10) :: acc)
        }
      }

      multiplyByDigitTailrec(number, 0, List())
    }

    def addTwoNumbers(a: List[Int], b: List[Int]): List[Int] = {
      def addTwoTailrec(remainingA: List[Int], remainingB: List[Int], carry: Int = 0, acc: List[Int] = List()): List[Int] = {
        if (remainingA.isEmpty && remainingB.isEmpty) {
          if (carry == 0) acc.reverse
          else (carry :: acc).reverse
        } else if (remainingA.isEmpty) {
          acc.reverse ++ addTwoTailrec(List(carry), remainingB)
        } else if (remainingB.isEmpty) {
          acc.reverse ++ addTwoTailrec(List(carry), remainingA)
        } else {
          val newSum = remainingA.head + remainingB.head + carry
          val newDigit = newSum % 10
          val newCarry = newSum / 10

          addTwoTailrec(remainingA.tail, remainingB.tail, newCarry, newDigit :: acc)
        }
      }

      if (a.isEmpty) b
      else if (b.isEmpty) a
      else addTwoTailrec(a, b)
    }

    def multiplyDigits(a: List[Int], b: List[Int]): List[Int] =
      b.zipWithIndex.map { case (digit, index) =>
        List.fill(index)(0) ++ multiplyByDigit(a, digit)
      }
        .reduce(addTwoNumbers)

    val digitsA = a.reverse.map(_ - '0').toList
    val digitsB = b.reverse.map(_ - '0').toList
    val digitsResult = multiplyDigits(digitsA, digitsB)

    val result = digitsResult.reverse.mkString

    if (result.isEmpty || result.charAt(0) == '0') "0"
    else result
  }

  println("my test")
  println(multiplyStrings(Int.MaxValue.toString, "22"))
  println(multiplyStrings("1024", "22"))
  println(multiplyStrings("123", "456"))
  println(multiplyStrings("125137859237859327893","45652378957234896"))

  println("\n Daniel test")
  println(multiplyStrings_v2("123","456"))
  println(multiplyStrings_v2("125137859237859327893","45652378957234896"))
}
