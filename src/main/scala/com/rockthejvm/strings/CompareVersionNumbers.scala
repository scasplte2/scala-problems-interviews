package com.rockthejvm.strings

import scala.annotation.tailrec

object CompareVersionNumbers extends App {
  /*
    Example: 0.9 < 1.0.3.4 < 1.1.0 < 2.0 < 2.1 == 2.01

    -1: Version1 < version2
    0: version1 == version2
    1: version1 > version2

   */
  def compareVersionNumbers(version1: String, version2: String): Int = {
    val v1List = version1.split('.').map(_.toInt).toList
    val v2List = version2.split('.').map(_.toInt).toList

    @tailrec
    def compareTailrec(v1List: List[Int], v2List: List[Int]): Int = {
      if (v1List.isEmpty && v2List.isEmpty) 0
      else if (v1List.isEmpty) {
        if (v2List.exists(_ != 0)) -1
        else 0
      }
      else if (v2List.isEmpty) {
        if (v1List.exists(_ != 0)) 1
        else 0
      }
      else {
        val r1 = v1List.head
        val r2 = v2List.head

        if (r1 < r2) -1
        else if (r1 > r2) 1
        else compareTailrec(v1List.tail, v2List.tail)
      }
    }

    compareTailrec(v1List, v2List)
  }

  println(compareVersionNumbers("1.0", "1.0.0.1"))
  println(compareVersionNumbers("0.9", "1.0.3.4"))
  println(compareVersionNumbers("1.0.3.4", "1.1.0"))
  println(compareVersionNumbers("2.0", "1.1.0"))
  println(compareVersionNumbers("2.1", "2.01"))
}
