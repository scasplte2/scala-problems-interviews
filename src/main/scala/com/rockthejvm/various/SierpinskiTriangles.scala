package com.rockthejvm.various

import scala.annotation.tailrec

object SierpinskiTriangles extends App {

  /*
  n = 3 should give              n = 2              n = 1
            *                      *                  *
           * *                    * *                * *
          *   *                  *   *
         * * * *                * * * *
        *       *
       * *     * *
      *   *   *   *
     * * * * * * * *

   */
  def sierpinski(n: Int): String = {
    def sierpinskiStack(level: Int): List[String] = {
      if (level == 0) List("*")
      else {
        val triangle = sierpinskiStack(level - 1)
        val spaces = " " * (1 << (level - 1)) // 2^(n-1) spaces, the bitshift is a fancy fast way to do the exponentiation
        val topTriangle = triangle.map(spaces + _ + spaces)
        val bottomTriangles = triangle.map (row => row + " " + row)
        topTriangle ++ bottomTriangles
      }
    }

    @tailrec
    def sierpinskiTail(currentLevel: Int, currentTriangle: List[String]): List[String] = {
      if (currentLevel >= n) currentTriangle
      else {
        val spaces = " " * (1 << currentLevel)
        val topTriangle = currentTriangle.map(spaces + _ + spaces)
        val bottomTriangles = currentTriangle.map(row => row + " " + row)
        sierpinskiTail(currentLevel + 1, topTriangle ++ bottomTriangles)
      }

    }

    sierpinskiTail(0, List("*")).mkString("\n")
//    sierpinskiStack(n).mkString("\n")
  }

//  println(sierpinski(1))
//  println(sierpinski(2))
//  println(sierpinski(3))
//  println(sierpinski(6))
  println(sierpinski(8))


}
