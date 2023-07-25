package com.rockthejvm.numbers

import scala.annotation.tailrec
import scala.language.postfixOps

object ParseInteger extends App {

  /*
  Return a number from the string argument:
  - there may be leading spaces, ignore those
  - read the sign character if present
  - read all the digits until the end of the string or until a non-digit character
  - return the number formed from those digits
  - if the number exceeds the Int range, return either Int.MinValue (underflow) or Int.MaxValue (overflow)

  "    +1234 is the number I want" => 1234
   */
  // My solution
  def parseInteger(str: String): Int = {
    val validChars = List('-', '+', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0')
    val filteredString = str.filter(validChars.contains(_))

    if (filteredString.isEmpty) 0
    else {
      filteredString.toIntOption match {
        case Some(value) => value
        case None if (filteredString.charAt(0) == '-') => Int.MinValue
        case None => Int.MaxValue
      }
    }
  }

  // Daniel's solution
  def parseInteger_v2(str: String): Int = {
    val WHITESPACE = ' '
    val PLUS = '+'
    val MINUS = '-'
    val DIGITS = "0123456789".toSet

    def integerRangeEnd(sign: Int): Int = if (sign >= 0) Int.MaxValue else Int.MinValue

    @tailrec
    def parseTailrec(remainder: String, sign: Int, acc: Int): Int = {
      if (remainder.isEmpty || !DIGITS.contains(remainder.charAt(0))) acc
      else {
        val newDigit = remainder.charAt(0) - '0'
        val tentativeResult = acc * 10 + newDigit * sign

        if((sign >= 0) != (tentativeResult >= 0 )) integerRangeEnd(sign)
        else parseTailrec(remainder.substring(1), sign, tentativeResult)
      }
    }

    if (str.isEmpty) 0
    else if (str.charAt(0) == WHITESPACE) parseInteger_v2(str.substring(1))
    else if (str.charAt(0) == PLUS) parseTailrec(str.substring(1), 1, 0)
    else if (str.charAt(0) == MINUS) parseTailrec(str.substring(1), -1, 0)
    else parseTailrec(str, 1, 0)
  }

  println("\n Mine")
  println(parseInteger("    +1234 is the number I want"))
  println(parseInteger("    -1234 is the number I want"))
  println(parseInteger(""))
  println(parseInteger("String"))
  println(parseInteger("1"))
  println(parseInteger("-1"))
  println(parseInteger("    Scala"))
  println(parseInteger("    4256"))
  println(parseInteger("    -4256"))
  println(parseInteger("    +4256"))
  println(parseInteger(Int.MaxValue.toString))
  println(parseInteger(Int.MinValue.toString))
  println(parseInteger("452324148646354324132154654"))
  println(parseInteger("-452324148646354324132154654"))

  println("\n Daniels")
  println(parseInteger_v2("    +1234 is the number I want"))
  println(parseInteger_v2("    -1234 is the number I want"))
  println(parseInteger_v2(""))
  println(parseInteger_v2("String"))
  println(parseInteger_v2("1"))
  println(parseInteger_v2("-1"))
  println(parseInteger_v2("    Scala"))
  println(parseInteger_v2("    4256"))
  println(parseInteger_v2("    -4256"))
  println(parseInteger_v2("    +4256"))
  println(parseInteger_v2(Int.MaxValue.toString))
  println(parseInteger_v2(Int.MinValue.toString))
  println(parseInteger_v2("452324148646354324132154654"))
  println(parseInteger_v2("-452324148646354324132154654"))
}
