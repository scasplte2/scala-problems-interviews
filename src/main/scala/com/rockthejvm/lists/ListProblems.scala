package com.rockthejvm.lists

import scala.annotation.tailrec
import scala.jdk.Accumulator

// Our learning list
sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  // for a more functional approach
  // def headOption: Option[T]

  /**
    * Easy problems
    */

  // get element at given index
  def apply(index: Int): T

  // return the size of the list
  def length: Int

  // return the list in reversed order
  def reverse: RList[T]

  // concatenate another list to this list
  def ++[S >: T](anotherList: RList[S]): RList[S]

  // remove an element at a given index, return a new list
  def removeAt(index: Int): RList[T]

  def map[S](f: T => S): RList[S]
  def flatMap[S](f: T => RList[S]): RList[S]
  def filter(f: T => Boolean): RList[T]

  /**
    * Medium difficulty problems
    */

  // run-length encoding
  def rle: RList[(T, Int)]

  // duplicate the elements of a list a given number of times
  def duplicateEach(k: Int): RList[T]

  // cycle elements a number of positions to the left
  def rotate(k: Int): RList[T]

  // random sampling of K elements into a new list
  // K can be arbitrary (smaller or larger than list length)
  // elements do not have to be unique
  def sample(k: Int): RList[T]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException // side effect
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  // override def headOption: Option[Nothing] = None
  override def toString: String = "[]"
  override def apply(index: Int): Nothing = throw new NoSuchElementException
  override def length: Int = 0
  override def reverse: RList[Nothing] = RNil
  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList
  override def removeAt(index: Int): RList[Nothing] =  RNil //hmm... why not NoSuchElement?
  override def map[S](f: Nothing => S): RList[S] = RNil
  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil
  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil
  override def rle: RList[(Nothing, Int)] = RNil
  override def duplicateEach(k: Int): RList[Nothing] = RNil
  override def rotate(k: Int): RList[Nothing] = RNil
  override def sample(k: Int): RList[Nothing] = RNil
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

  override def length: Int = {
    // my original idea, the If statement is likely more performant due to not having to unbox?
//    @tailrec
//    def lengthTailrec(remaining: RList[T], iteration: Int): Int =
//      remaining match {
//        case RNil => iteration
//        case ::(head, tail) => lengthTailrec(tail, iteration + 1)
//      }

    // complexity is O(N) where N is the length of the list
//     Daniels impl
    @tailrec
    def lengthTailrec(remaining: RList[T], iteration: Int): Int = {
      if (remaining.isEmpty) iteration
      else lengthTailrec(remaining.tail, iteration + 1)
    }

    lengthTailrec(this, 0)
  }

  override def reverse: RList[T] = {
    // 1, 2, 3, Nil
    // 2, 3 - 1, Nil
    // 3 - 2, 1, Nil

    /*
    [1,2,3,4].reverse = reverseTailRec([1,2,3,4], RNil)
    = reverseTailrec([2,3,4], [1])
    = reverseTailrec([3,4], [2,3])
    = reverseTailrec([4], [3,2,1])
    = reverseTailrec([], [4,3,2,1])
    = [4,3,2,1]
     */
    // complexity is O(N)
    @tailrec
    def reverseTailRec(remaining: RList[T], result: RList[T]): RList[T] = {
      if (remaining.isEmpty) result
      else reverseTailRec(remaining.tail, remaining.head :: result)
    }

    reverseTailRec(this, RNil)
  }

  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    /*
    [1,2,3] ++ [4,5] = concatTailrec([3,2,1], [4,5])
    = concatTailrec([2,1], [3,4,5])
    = concatTailrec([[1], [2,3,4,5])
    = concatTailrec([], [1,2,3,4,5])
    = [1,2,3,4,5]
     */
    @tailrec
    def concatTailrec(remaining: RList[S], result: RList[S]): RList[S] = {
      if (remaining.isEmpty) result
      else concatTailrec(remaining.tail, remaining.head :: result)
    }

    concatTailrec(this.reverse, anotherList)
  }

  override def removeAt(index: Int): RList[T] = {
    /*
    [1,2,3,4,5].removeAt(2) = removeAtTailrec([1,2,3,4,5], [], 0)
    = removeAtTailrec([2,3,4,5], [1], 1)
    = removeAt([3,4,5], [2,1], 2) -> this is where iteration == index so skip over remaining.head and concat
    = [2,1].reverse ++ [4,5]
     */
    @tailrec
    def removeAtTailrec(remaining: RList[T], acc: RList[T], iteration: Int): RList[T] = {
      if (iteration == index) acc.reverse ++ remaining.tail
      else if (remaining.isEmpty) acc.reverse
      else removeAtTailrec(remaining.tail, remaining.head :: acc, iteration + 1)
    }

    if (index < 0) this
    else removeAtTailrec(this, RNil, 0)
  }

  override def map[S](f: T => S): RList[S] = {
    /*
    [1,2,3].map(x => x + 1) = mapTailrec([1,2,3], [])
    = mapTailrec([2,3], [2])
    = mapTailrec([3], [3, 2])
    = mapTailrec([], [4,3,2])
    = [4,3,2].reverse = [2,3,4]
     */
    @tailrec
    def mapTailrec(remaining: RList[T], result: RList[S]): RList[S] = {
      if (remaining.isEmpty) result.reverse
      else mapTailrec(remaining.tail, f(remaining.head) :: result)
    }

    mapTailrec(this, RNil)
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {
    /*
    [1,2,3].flatMap(x => [x, 2 * x]) = flatMapTailrec([1,2,3], [])
    = flatMapTailrec([2,3], [1,2].reverse)
    = flatMapTailrec([3], [2,4].reverse ++ [2,1])
    = flatMapTailrec([], [3,6].reverse ++ [4,2,2,1]
    = [6,3,4,2,2,1].reverse = [1,2,2,4,3,6]

    Complexity:
    N is length of outer list
    M is length of inner list
    -- guess O(M * N^2)
    -- ans O(M^2)
    problematic since it takes too long, will return too
     */
//    @tailrec
//    def flatMapTailrec(remaining: RList[T], result: RList[S]): RList[S] = {
//      if (remaining.isEmpty) result.reverse
//      else flatMapTailrec(remaining.tail, f(remaining.head).reverse ++ result)
//    }
//    flatMapTailrec(this, RNil)

    /*
    [1,2,3].flatMap(x => [x, 2 * x]) = flatMapTailrec_v2([3,2,1], [])
    = flatMapTailrec([2,1], [3,6])
    = flatMapTailrec([1], [2,4] ++ [3,6])
    = flatMapTailrec([], [1,2] ++ [2,4,3,6]
    = [1,2,2,4,3,6]
     */

//    // my attempt
//    def flatMapTailrec_v3(): RList[S] = {
//      val accumulator = this.map(f) // get all the results of applying f
//      concatenateAll(accumulator, RNil, RNil) // concatenate into one big list
//    }
//    flatMapTailrec_v3()
//
    @tailrec
    def flatMapTailrec_v2(remaining: RList[T], accumulator: RList[RList[S]]): RList[S] = {
      if (remaining.isEmpty) concatenateAll(accumulator, RNil, RNil)
      else flatMapTailrec_v2(remaining.tail, f(remaining.head).reverse :: accumulator)
    }

    @tailrec
    def concatenateAll(remaining: RList[RList[S]], current: RList[S], result: RList[S]): RList[S] = {
      if (remaining.isEmpty && current.isEmpty) result
      else if (current.isEmpty) concatenateAll(remaining.tail, remaining.head, result)
      else concatenateAll(remaining, current.tail, current.head :: result)
    }

    flatMapTailrec_v2(this, RNil :: RNil)
  }

  override def filter(f: T => Boolean): RList[T] = {
    @tailrec
    def filterTailrec(remaining: RList[T], result: RList[T]): RList[T] = {
      if (remaining.isEmpty) result.reverse
      else if (f(remaining.head)) filterTailrec(remaining.tail, remaining.head :: result)
      else filterTailrec(remaining.tail, result)
    }

    filterTailrec(this, RNil)
  }

  override def rle: RList[(T, Int)] = {
    /*
    [1,1,2,2,3].rle = rleTailrec([1,1,2,2,3], 1, 0, [])
    = rleTailrec([1,2,2,3], 1, 1, [])
    = rleTailrec([2,2,3], 1, 2, [])
    = rleTailrec([2,3], 2, 1, [(1, 2)])
    = rleTailrec([3], 2, 2, [(1, 2)])
    = rleTailrec([], 3, 1, [(2, 2), (1, 2)])
    = (3, 2) :: [(2, 2), (1, 2)]

    Complexity - O(N)
     */
    @tailrec
    def rleTailrec(remaining: RList[T], value: T, count: Int, result: RList[(T, Int)]): RList[(T, Int)] = {
      if (remaining.isEmpty && count == 0) result
      else if (remaining.isEmpty) (value, count) :: result
      else if (remaining.head == value) rleTailrec(remaining.tail, value, count + 1, result)
      else rleTailrec(remaining.tail, remaining.head, 1, (value, count) :: result)
    }

    rleTailrec(this, this.head, 0, RNil).reverse
  }

  override def duplicateEach(k: Int): RList[T] = {
    /*
    [1,2,3].duplicateEach(2) = detr([1,2,3], 1, [])
    = detr([1,2,3], 2, [1])
    = detr([2,3], 1, [1,1])
    = detr([2,3], 2, [2,1,1])
    ...

    Complexity - O(N * K)
     */
    @tailrec
    def duplicateEachTailrec(remaining: RList[T], iteration: Int, result: RList[T]): RList[T] = {
      if (remaining.isEmpty) result.reverse
      else if (iteration < k) duplicateEachTailrec(remaining, iteration + 1, remaining.head :: result)
      else duplicateEachTailrec(remaining.tail, 1, remaining.head :: result)
    }

    duplicateEachTailrec(this, 1, RNil)
  }

  override def rotate(k: Int): RList[T] = {
    /*
    [1,2,3,4].rotate(2) = rotateTailrec([1,2,3,4], [], 6)
    = rotateTailrec([2,3,4], [1], 5)
    = rotateTailrec([3,4], [2,1], 4)
    = rotateTailrec([4], [3,2,1], 3)
    = rotateTailrec([], [4,3,2,1], 2)
    = rotateTailrec([1,2,3,4], [], 2)
    = rotateTailrec([2,3,4], [1], 1)
    = rotateTailrec([3,4], [2,1], 0)
    = [3,4] ++ [2,1].reverse
    = [3,4,1,2]

    Complexity: O(max(N, K))
     */

    @tailrec
    def rotateTailrec(remaining: RList[T], accumulator: RList[T], iterations: Int): RList[T] = {
      if (remaining.isEmpty && iterations == 0) this // terminate if k is some multiple of the list length
      else if (iterations == 0) remaining ++ accumulator.reverse // terminate if k is less then the list length
      else if (remaining.isEmpty) rotateTailrec(this, RNil, iterations) // continue if k > length
      else rotateTailrec(remaining.tail, remaining.head :: accumulator, iterations - 1)
    }

    rotateTailrec(this, RNil, k)
  }

  override def sample(k: Int): RList[T] = {
    val random = new scala.util.Random(System.currentTimeMillis())
    val length = this.length

    // Complexity - O(N * K)
    @tailrec
    def sampleTailrec(iteration: Int, accumulator: RList[T]): RList[T] = {
      if (iteration == k) accumulator
      else sampleTailrec(
        iteration + 1,
        this.apply(random.nextInt(length)) :: accumulator
      )
    }

    // a second method (still tail recursive) for sampling
    val sample_v2: RList[T] =
      RList.from(1 to k).map(_ => this.apply(random.nextInt(length)))

    if (k < 0) RNil
    //else sampleTailrec(0, RNil)
    else sample_v2
  }
}

object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec
    def convertToRListTailrec(remaining: Iterable[T], result: RList[T]): RList[T] = {
      if (remaining.isEmpty) result
      else convertToRListTailrec(remaining.tail, remaining.head :: result)
    }

    convertToRListTailrec(iterable, RNil).reverse
  }
}



// review of covariance
//class Animal
//class Dog extends Animal
//List[Dog] is also a List[Animal]

object ListProblems extends App {

  val aSmallList = ::(1, ::(2, ::(3, RNil)))
  val aLargeList = RList.from(1 to 10000)

  def testEasy(): Unit = {
    val aSmallList_2 = 1 :: 2 :: 3 :: RNil // RNil.::(3).::(2).::(1)
    println("For small lists")
    println(aSmallList)
    println(s"The value at index 1 is ${aSmallList(1)}")
    //  println(aSmallList(5))
    println(s"The length of the list is ${aSmallList.length}")
    println(s"This is the reversed list ${aSmallList.reverse}")
    println(s"This is the concat of two list ${aSmallList ++ (4 :: 5 :: RNil)}")
    println(s"This is the list with the 2 index removed ${aSmallList.removeAt(2)}")
    println(s"This is a list with the 4 index removed ${RList.from(1 to 10).removeAt(4)}")

    // test RList.from
    val aLargeList = RList.from(1 to 10000)
    println("\nFor large lists")
    println(s"The value at index 1 is ${aLargeList(1338)}")
    println(s"The length of the list is ${aLargeList.length}")
    println(s"This is the reversed list ${aLargeList.reverse}")

    println(s"Add 1 to the list ${aSmallList.map(_ + 1)}")
    println(s"Test without flatMap ${aSmallList.map(i => aSmallList.map(_ * i))}")
    println(s"Test flatMap ${aSmallList.flatMap(i => aSmallList.map(_ * i))}")
    println(s"Test filter to remove evens ${RList.from(1 to 10).filter(i => (i % 2) == 1)}")
  }
//  testEasy()

  def testMedium(): Unit = {
    println(s"Test of RLE on [1,1,2,2,3] -> ${(1 :: 1 :: 2 :: 2 :: 3 :: RNil).rle}")
    println(s"\nDuplicateEach on [1,2,3] -> ${aSmallList.duplicateEach(3)}")
    println(s"\nRotate by 1 on [1,2,3,4] -> ${(1 :: 2 :: 3 :: 4 :: RNil).rotate(1)}")
    println(s"Rotate by 5 on [1,2,3,4] -> ${(1 :: 2 :: 3 :: 4 :: RNil).rotate(5)}")
    println(s"Rotate by 3 on [1,2,3,4] -> ${(1 :: 2 :: 3 :: 4 :: RNil).rotate(3)}")
    println(s"Rotate by 7 on [1,2,3,4] -> ${(1 :: 2 :: 3 :: 4 :: RNil).rotate(7)}")
    println(s"Rotate by 4 on [1,2,3,4] -> ${(1 :: 2 :: 3 :: 4 :: RNil).rotate(4)}")
    println(s"Rotate by 8 on [1,2,3,4] -> ${(1 :: 2 :: 3 :: 4 :: RNil).rotate(8)}")
    println(s"\nSample 3 from [1,2,3,4] -> ${(1 :: 2 :: 3 :: 4 :: RNil).sample(3)}")
    println(s"\nFlatmap_v2 on [1,2,3] -> ${RList.from(1 to 3).flatMap(x => x :: 2*x :: RNil)}")

    val time = System.currentTimeMillis()
    aLargeList.flatMap(x => x :: 2*x :: RNil)
    println(s"time taken ${System.currentTimeMillis() - time}")
  }
  testMedium()




}
