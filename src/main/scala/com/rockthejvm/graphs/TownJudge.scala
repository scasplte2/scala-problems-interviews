package com.rockthejvm.graphs

object TownJudge extends App {

  /*
  n people, 1 to n
  trust = List[(a,b)] -> (a,b) = a trust b

  There might be a town judge.
    The town judge trusts nobody -> outdegree of judge is 0
    Everybody (except for the town judge) trusts the town judge -> indegree of judge is n - 1
    There is exactly one person that satisfies these properties

   Find the town judge or return -1


   ex. n = 3, [(1,2)(3,2)(1,3)]
   */
  def findJudge(n: Int, trust: List[(Int, Int)]): Int = {
    val inDegrees = trust.foldLeft(Map[Int, Int]()) { case (acc, (_,b)) => acc + (b -> (acc.getOrElse(b, 0) + 1)) }
    val outDegrees = trust.foldLeft(Map[Int, Int]()) { case (acc, (a,_)) => acc + (a -> (acc.getOrElse(a, 0) + 1)) }

    (1 to n).find { person =>
      inDegrees.getOrElse(person, 0) == n -1 && outDegrees.getOrElse(person, 0) == 0
    }.getOrElse(-1)
  }

  println(findJudge(3, List((1,2),(3,2),(1,3))))
}
