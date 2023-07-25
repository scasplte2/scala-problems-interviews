package com.rockthejvm.numbers

import scala.annotation.tailrec

object ReverseInteger extends App {
  // return a number with the digits reversed
  // if the result overflows Int, return 0
  /*
  reverseInteger(123) = 321
   */
  // my solution
  def reverseInteger(number: Int): Int = {
    val signOfNum = number.sign
    val reversedLong = Math.abs(number.toLong).toString.reverse.toLong
    if (reversedLong > Int.MaxValue) 0
    else signOfNum * reversedLong.toInt
  }

  // Daniel's solution
  def reverseInteger_v2(number: Int): Int = {
    // assumes positive args
    @tailrec
    def reverseTailrec(remaining: Int, acc: Int): Int = {
      if (remaining == 0) acc
      else {
        val digit = remaining % 10
        val tentativeResult = acc * 10 + digit

        if ((acc >= 0) != (tentativeResult >= 0)) 0
        else reverseTailrec(remaining / 10, tentativeResult)
      }
    }

    if (number >= 0) reverseTailrec(number, 0)
    else -reverseTailrec(-number, 0)
  }

  println(reverseInteger(123))
  println(reverseInteger(-123))
  println(reverseInteger(1234567898))
  println(reverseInteger(0))
  println(reverseInteger(-10))
  println(reverseInteger(Int.MinValue/20))
  println(reverseInteger(Int.MaxValue))

  println("\n")
  println(reverseInteger_v2(123))
  println(reverseInteger_v2(-123))
  println(reverseInteger_v2(1234567898))
  println(reverseInteger_v2(0))
  println(reverseInteger_v2(-10))
  println(reverseInteger_v2(Int.MinValue / 20))
  println(reverseInteger_v2(Int.MaxValue))

  println("\n daniel - positive")
  println(reverseInteger_v2(0)) // 0
  println(reverseInteger_v2(9)) // 9
  println(reverseInteger_v2(53)) // 35
  println(reverseInteger_v2(504)) //405
  println(reverseInteger_v2(540)) //45
  println(reverseInteger_v2(53678534)) //43587635
  println(reverseInteger_v2(Int.MaxValue)) // 0
  println("\n daniel - negative")
  println(reverseInteger_v2(-9))
  println(reverseInteger_v2(-53))
  println(reverseInteger_v2(-504))
  println(reverseInteger_v2(-540))
  println(reverseInteger_v2(-53678534))
  println(reverseInteger_v2(Int.MinValue))

}
