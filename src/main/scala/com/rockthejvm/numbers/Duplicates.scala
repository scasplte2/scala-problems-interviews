package com.rockthejvm.numbers

import scala.annotation.tailrec
import scala.collection.immutable.Map

object Duplicates extends App {

  // all numbers in the list appear EXACTLY twice, EXCEPT for one
  // return the number that occurs only once

  // my impl
  def duplicates(list: List[Int]): Int = {
    /*
    duplicates([1,2,3,4,1,2,3])
    = dtr([2,3,4,1,2,3], 1, [])
    = dtr([3,4,1,2,3], 1, [2])
    = dtr([4,1,2,3], 1, [3,2])
    = dtr([1,2,3], 1, [4,3,2])
    = dtr(
     */

    // not optimal, complexity is O(N^2)
    @tailrec
    def duplicateTailrec(remaining: List[Int], current: Int, buffer: List[Int]): Int = {
      if (remaining.isEmpty) current
      else if (remaining.head == current) {
        val newList = remaining.tail ++ buffer
        duplicateTailrec(newList.tail, newList.head, List())
      } else duplicateTailrec(remaining.tail, current, remaining.head :: buffer)
    }

    duplicateTailrec(list.tail, list.head, List())
  }

  // Daniel's impl
  def duplicates_v2(list: List[Int]): Int = {
    @tailrec
    def duplicatesTailrec(remaining: List[Int], occurences: Map[Int, Int] = Map()): Int = {
      if (remaining.isEmpty) occurences.filter { case (elem, occur) => occur == 1 }.head._1
      else {
        val currentNumber = remaining.head
        val currentOccurence = occurences.getOrElse(currentNumber, 0)
        duplicatesTailrec(remaining.tail, occurences + (currentNumber -> (currentOccurence + 1)))
      }
    }

    duplicatesTailrec(list)
  }
  def duplicates_v3(list: List[Int]): Int = {
    @tailrec
    def duplicatesTailrec(remaining: List[Int], memory: Set[Int] = Set()): Int = {
      if (remaining.isEmpty) memory.head
      else if (memory.contains(remaining.head)) duplicatesTailrec(remaining.tail, memory - remaining.head)
      else duplicatesTailrec(remaining.tail, memory + remaining.head)
    }

    duplicatesTailrec(list)
  }

  // optimal solution
  // ^ = XOR
  // Complexity: O(N) time, O(1) space
  def duplicates_v4(list: List[Int]): Int = list.foldLeft(0){ case(acc, elem) => acc ^ elem }

  val bigRange = (1 to 100000).toList
  val bigRangeDup = bigRange ++ List(523495345) ++ bigRange

  println("my impl")
  println(duplicates(List(1,2,3,4,1,2,3)))
  println(duplicates(List(6,4,2,1,3,6,4,2,1,3,7)))
  println(duplicates(List(6,4,2,1,3,7,4,2,1,3,7)))
  //println(duplicates(bigRangeDup))

  println("\nv2")
  println(duplicates_v2(List(1, 2, 3, 4, 1, 2, 3)))
  println(duplicates_v2(List(6, 4, 2, 1, 3, 6, 4, 2, 1, 3, 7)))
  println(duplicates_v2(List(6, 4, 2, 1, 3, 7, 4, 2, 1, 3, 7)))
  println(duplicates_v2(bigRangeDup))

  println("\nv3")
  println(duplicates_v3(List(1, 2, 3, 4, 1, 2, 3)))
  println(duplicates_v3(List(6, 4, 2, 1, 3, 6, 4, 2, 1, 3, 7)))
  println(duplicates_v3(List(6, 4, 2, 1, 3, 7, 4, 2, 1, 3, 7)))
  println(duplicates_v3(bigRangeDup))

  println("\nv4")
  println(duplicates_v4(List(1, 2, 3, 4, 1, 2, 3)))
  println(duplicates_v4(List(6, 4, 2, 1, 3, 6, 4, 2, 1, 3, 7)))
  println(duplicates_v4(List(6, 4, 2, 1, 3, 7, 4, 2, 1, 3, 7)))
  println(duplicates_v4(bigRangeDup))

}
