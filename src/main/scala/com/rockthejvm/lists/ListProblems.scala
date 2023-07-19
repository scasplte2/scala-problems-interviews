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
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException // side effect
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"

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
}



// review of covariance
//class Animal
//class Dog extends Animal
//List[Dog] is also a List[Animal]

object ListProblems extends App {

  val aSmallList = ::(1, ::(2, ::( 3, RNil)))
  val aSmallList_2 = 1 :: 2 :: 3 :: RNil // RNil.::(3).::(2).::(1)
  println(aSmallList)

}
