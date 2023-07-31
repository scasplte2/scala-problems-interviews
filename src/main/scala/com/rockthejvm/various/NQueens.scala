package com.rockthejvm.various

import scala.annotation.tailrec

object NQueens extends App {

  def nQueens(n: Int): List[String] = {
    // returns true if the position can be attacked by a queen in the queens list
    def conflict(position: Int, queens: List[Int]): Boolean = {
      def conflictOneQueen(position: Int, queen: Int, index: Int): Boolean =
        queen == position || (index + 1) == (position - queen) || (index + 1) == (queen - position)

      queens.zipWithIndex.exists { case (queen, index) =>
        conflictOneQueen(position, queen, index)
      }
    }

    /*
    nQueens(4) =
    nqtr(0, [], [])
    nqtr(0, [0], [])
    nqtr(1, [0], [])
    nqtr(2, [0], [])
    nqtr(0, [2,0], [])
    nqtr(1, [2,0], [])
    nqtr(2, [2,0], [])
    nqtr(3, [2,0], [])
    nqtr(4, [2,0], [])
    nqtr(3, [0], [])
    nqtr(0, [3,0], [])
    nqtr(1, [3,0], [])
    nqtr(0, [1,3,0], [])
    nqtr(1, [1,3,0], [])
    nqtr(2, [1,3,0], [])
    nqtr(3, [1,3,0], [])
    nqtr(4, [1,3,0], [])
    nqtr(2, [3,0], [])
    nqtr(3, [3,0], [])
    nqtr(4, [3,0], [])
    nqtr(4, [0], [])
    nqtr(1, [], [])
    ... continues for awhile

     */
    @tailrec
    def nQueensTailrec(currentPosition: Int, currentQueens: List[Int], solutions: List[List[Int]]): List[List[Int]] = {
      if (currentPosition >= n && currentQueens.isEmpty) {
        // I'm out of options
        solutions
      } else if (currentPosition >= n) {
        // I'm out of options of this row; move the previous queen by 1
        nQueensTailrec(currentQueens.head + 1, currentQueens.tail, solutions)
      } else if (conflict(currentPosition, currentQueens)) {
        // conflict with the others queen, try next position
        nQueensTailrec(currentPosition + 1, currentQueens, solutions)
      } else if (currentQueens.length == n - 1) {
        // I've just built a solution
        val newSolution = currentPosition :: currentQueens
        nQueensTailrec(currentPosition + 1, currentQueens, newSolution :: solutions)
      } else {
        // try next queen on the next row since this one is valid
        nQueensTailrec(0, currentPosition :: currentQueens, solutions)
      }
    }

    def prettyPrint(solution: List[Int]): String = {
      val topEdge = (1 to n).map(_ => "_").mkString(".",".",".")
      val rows = solution.map { queen =>
        val cellsBefore = (0 until queen).map(_ => "_")
        val beforeString = if (cellsBefore.isEmpty) "|" else cellsBefore.mkString("|","|","|")
        val cellsAfter = ((queen + 1) until n).map(_ => "_")
        val afterString = if (cellsAfter.isEmpty) "|" else cellsAfter.mkString("|","|","|")

        beforeString + "X" + afterString
      }

      s"$topEdge\n${rows.mkString("\n")}"
    }

    nQueensTailrec(0, List(), List()).map(prettyPrint)
  }

  val q8 = nQueens(8)
  val printableSolutions = q8.mkString("\n\n")

  println(printableSolutions)
  println(s"Total number of solutions = ${q8.length}")

//  nQueens(4).foreach(println)

}
