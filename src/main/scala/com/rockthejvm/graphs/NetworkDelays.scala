package com.rockthejvm.graphs

import com.rockthejvm.graphs.GraphProblems.Graph

import scala.annotation.tailrec

object NetworkDelays extends App {

  /*
  n - number of nodes
  latencies - list((a,b,t)) -> where (a,b,t) denotes the latency, t, between node a and node b
  source - starting node for signal

  What is the time it takes for a signal originating from source to reach ALL other nodes in the network?

  ex.
  n = 4
  latencies = [(1,2,3),(1,3,10),(1,4,10),(2,3,4),(3,4,2)]
  source = 1
   */
  def computeNetworkDelay(n: Int, latencies: List[(Int, Int, Int)], source: Int): Int = {
    // adjacency list / graph
    val edgeGraph: Graph[Int] = latencies.foldLeft(Map[Int, Set[Int]]()){ case (acc, (node, neighbor, _)) =>
      acc + (node -> (acc.getOrElse(node, Set()) + neighbor))
    }

    // adjacency matrix
    val weights: Map[(Int, Int), Int] = latencies.map {
      case (a,b,t) => ((a,b), t)
    }.toMap

    /*
    for { 1 -> [(2,3), (3,10), (4,10)], 2 -> [(3,4)], 3 -> [(4,2)], 4 -> [] }
    edgeGraph = { 1 -> [2,3,4], 2 -> [3], 3 -> [4] }
    weights = { (1,2) -> 3, (1,3) -> 10, (1,4) -> 10, (2,3) -> 4, (3,4) -> 2 }

    dtr([1], [], { 1 -> 0, 2 -> MAX, 3 -> MAX, 4 -> MAX })
    node = 1
    neighborCosts = { 2 -> 3, 3 -> 10, 4 -> 10 }
    unvisited = [2,3,4]

    dtr([2,3,4], [1], { 1 -> 0, 2 -> 3, 3 -> 10, 4 -> 10 })
    node = 2
    neighborCosts = { 3 -> 7 }
    unvisited = [3]

    dtr([3,4], [1,2], { 1 -> 0, 2 -> 3, 3 -> 7, 4 -> 10 })
    node = 3
    neighborCosts = { 4 -> 9 }
    unvisited = [4]

     dtr([4], [1,2,3], { 1 -> 0, 2 -> 3, 3 -> 7, 4 -> 9 })
     node = 4
     neighborCosts = {}
     unvisited = []

     dtr([], [1,2,3,4], { 1 -> 0, 2 -> 3, 3 -> 7, 4 -> 9 })
     = { 1 -> 0, 2 -> 3, 3 -> 7, 4 -> 9 }

     Complexity: O(N^2) due to minBy
     Best complexity could be O(N + E * log(N))
     */
    @tailrec
    def dijkstraTailrec(expanding: Set[Int], visited: Set[Int], costs: Map[Int, Int]): Map[Int, Int] = {
      if (expanding.isEmpty) costs
      else {
        val node = expanding.minBy(costs) // takes minimum cost from expanding set
        val neighborCosts: Map[Int, Int] = edgeGraph.getOrElse(node, Set()).map { neighbor =>
          val currentCost = costs.getOrElse(neighbor, Int.MaxValue)
          val tentativeCost = costs(node) + weights((node, neighbor))
          val bestCost = Math.min(currentCost, tentativeCost)

          (neighbor, bestCost)
        }.toMap

        val unvisitedNeighbors = neighborCosts.keySet.filterNot(visited) // all the nodes that have not been visited
        dijkstraTailrec(expanding - node ++ unvisitedNeighbors, visited + node, costs ++ neighborCosts)
      }
    }

    val initialCosts = (1 to n).map((_, Int.MaxValue)).toMap + (source -> 0)
    val maxLatency = dijkstraTailrec(Set(source), Set(), initialCosts).values.max

    if (maxLatency == Int.MaxValue) -1
    else maxLatency
  }

  println(computeNetworkDelay(2, List((1,2,1)), 2)) // -1
  println(computeNetworkDelay(2, List((1,2,1)), 1)) // 1
  println(computeNetworkDelay(4, List((2,1,1),(2,3,1),(3,4,1)), 2)) // 2
  println(computeNetworkDelay(4, List((1,2,3),(1,3,10),(1,4,10),(2,3,4),(3,4,2)), 1)) //9

}
