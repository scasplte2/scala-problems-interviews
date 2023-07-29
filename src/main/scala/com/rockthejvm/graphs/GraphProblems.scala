package com.rockthejvm.graphs

import scala.annotation.tailrec

object GraphProblems extends App {

  type Graph[T] = Map[T, Set[T]]

  val socialNetwork: Graph[String] = Map(
    "Alice" -> Set("Bob", "Charlie", "David"),
    "Bob" -> Set(),
    "Charlie" -> Set("David"),
    "David" -> Set("Bob", "Mary"),
    "Mary" -> Set("Bob", "Charlie")
  )

  /**
    * Easy problem
    */
  // number of nodes the given node is adjacent to
  def outDegree[T](graph: Graph[T], node: T): Int = graph.getOrElse(node, Set()).size

  // number of nodes pointing to the given node
  def inDegree[T](graph: Graph[T], node: T): Int = graph.values.count(_.contains(node))

  /**
    * Medium difficulty
    */
  // test whether there is a path from start to end in a given graph
  def isPath[T](graph: Graph[T], start: T, end: T): Boolean = {
    /*
    Alice -> Mary
    iptr([Alice], [])
    iptr([Bob, Charlie, David], [Alice])
    iptr([Charlie, David], [Alice, Bob])
    iptr([David, David], [Alice, Bob, Charlie]
    iptr([Bob, Mary, David], [Alice, Bob, Charlie, David])
    iptr([Mary, David], {alice, Bob, Charlie, David])
    = true

    Complexity: O(E) - E is number of edges
     */
    @tailrec
    def isPathTailrec(remaining: List[T], visited: Set[T]): Boolean = {
      if (remaining.isEmpty) false
      else {
        val node = remaining.head
        if (node == end) true
        else if (visited.contains(node)) isPathTailrec(remaining.tail, visited)
        else isPathTailrec(remaining.tail ++ graph(node), visited + node)
      }
    }

    if (graph.contains(start)) isPathTailrec(Set(start), Set())
    else false
  }

  def testEasy(): Unit = {
    println(outDegree(socialNetwork, "Alice")) //3
    println(inDegree(socialNetwork, "David")) //2
  }
//  testEasy()

  def testMedium(): Unit = {
    println(isPath(socialNetwork, "Alice", "Mary")) // true
    println(isPath(socialNetwork, "Mary", "Alice")) // false
  }
  testMedium()

}
