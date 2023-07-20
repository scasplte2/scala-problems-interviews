package com.rockthejvm.lists

import scala.annotation.tailrec

// Our learning list
sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  // for a more functional appraoch
  // def headOption: Option[T] // more functional

  //// begin of problems
  // time window 5 - 15 min, took me 9
  def apply(index: Int): T

  // time window 5 - 15 mins, took me about 4
  def length: Int
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException // side effect
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"
  override def apply(index: Int): Nothing = throw new NoSuchElementException
  override def length: Int = 0
  // override def headOption: Option[Nothing] = None
}

case class ::[+T](head: T, tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false
  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailrec(this, "") + "]"
  }

  override def apply(index: Int): T = {

    // complexity of this algorithm? (i.e. number of steps)
    // my thought - maximally it is the length of the list or the asked for index
    // Daniel - O(min(N, index))
    @tailrec
    def applyTailrec(remaining: RList[T], iteration: Int): T = {
      if (iteration == index) remaining.head
      else applyTailrec(remaining.tail, iteration + 1)
    }

    if (index < 0) throw new NoSuchElementException
    else applyTailrec(this, 0)
  }

  // complexity is O(N) where N is the length of the list
  override def length: Int = {

    // my original idea, the If statement is likely more performant due to not having to unbox?
//    @tailrec
//    def lengthTailrec(remaining: RList[T], iteration: Int): Int =
//      remaining match {
//        case RNil => iteration
//        case ::(head, tail) => lengthTailrec(tail, iteration + 1)
//      }

//     Daniels impl
    @tailrec
    def lengthTailrec(remaining: RList[T], iteration: Int): Int = {
      if (remaining.isEmpty) iteration
      else lengthTailrec(remaining.tail, iteration + 1)
    }

    lengthTailrec(this, 0)
  }
}



// review of covariance
//class Animal
//class Dog extends Animal
//List[Dog] is also a List[Animal]

object ListProblems extends App {

  val aSmallList = ::(1, ::(2, ::( 3, RNil)))
  val aSmallList_2 = 1 :: 2 :: 3 :: RNil // RNil.::(3).::(2).::(1)
  println(aSmallList)

  println(s"The value at index 1 is ${aSmallList(1)}")
//  println(aSmallList(5))

  println(s"The length of the list is ${aSmallList.length}")

}
