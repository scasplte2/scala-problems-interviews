package com.rockthejvm.strings

import scala.annotation.tailrec

object ParenthesesProblems extends App {
  /*
  does the string contain balanced parentheses?
  () => true
  ()() => true
  (()) => true
  )( => false
   */
  def hasValidParentheses(str: String): Boolean = {
    @tailrec
    def hasValidParenTailrec(remaining: String, accumulator: Int): Boolean = {
      if (remaining.isEmpty) accumulator == 0
      else if (remaining.charAt(0) == '(') hasValidParenTailrec(remaining.tail, accumulator + 1)
      else if (remaining.charAt(0) == ')' && accumulator > 0) hasValidParenTailrec(remaining.tail, accumulator - 1)
      else false
    }

    if (str.isEmpty) false
    else hasValidParenTailrec(str, 0)
  }

  def testHasValidParentheses(): Unit = {
    println(hasValidParentheses("()"))
    println(hasValidParentheses("()()"))
    println(hasValidParentheses("(())"))
    println(hasValidParentheses(")("))
    println(hasValidParentheses("((("))
    println(hasValidParentheses(")))"))
    println(hasValidParentheses(""))
  }
//  testHasValidParentheses()

  /*
  generate a list of strings of all balanced parentheses

  n = 1 -> List("()")
  n = 2 -> List("(())", "()()")
  n = 3 -> List("((()))", "()()()", "()(())", "(())()", "(()())")
   */

  def generateAllValidParentheses(n: Int): List[String] = {
    /*
    for n = 2
    () + () -> prepend ()
    ( + () + ) -> inject ()
    () + () -> append ()
     */
    @tailrec
    def genParensTailrec(nRemainingParens: Int, currentStrings: Set[String]): Set[String] = {
      if (nRemainingParens == 0) currentStrings
      else {
        val newStrings = for {
          string <- currentStrings
          index <- 0 until string.length
        } yield {
          val (before, after) = string.splitAt(index)
          s"$before()$after"
        }

        genParensTailrec(nRemainingParens - 1, newStrings)
      }
    }

    if (n <= 0) List()
    else genParensTailrec(n - 1, Set("()")).toList
  }

  def testGenerateAllValidParens(): Unit = {
    println(generateAllValidParentheses(1))
    println(generateAllValidParentheses(2))
    println(generateAllValidParentheses(3))
    println(generateAllValidParentheses(10))
  }
//  testGenerateAllValidParens()

}
