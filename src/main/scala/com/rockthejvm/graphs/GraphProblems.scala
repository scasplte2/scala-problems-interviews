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

    if (graph.contains(start)) isPathTailrec(List(start), Set())
    else false
  }

  // find and return ANY path from start to end in a given graph
  def findPath[T](graph: Graph[T], start: T, end: T): List[T] = {
    /*
    Alice -> Mary
    fptr([Alice], [])
    fptr([[Bob, Alice], [Charlie, Alice], [David, Alice]], [Alice]) // expand Alice to her adjacencies
    fptr([[Charlie, Alice], [David, Alice]], [Alice, Bob]) // Bob has no adjacent so put in visited
    fptr([[David, Charlie, Alice], [David, Alice]], [Alice, Bob, Charlie])
    fptr([[Bob, David, Charlie, Alice], [Mary, David, Charlie, Alice], [David, Alice]], [Alice, Bob, Charlie, David])
    fptr([[Mary, David, Charlie, Alice], [David, Alice]], [Alice, Bob, Charlie, David]) // Bob in expanded so skip
    fptr([[Mary, David, Charlie, Alice], [David, Alice]], [Alice, Bob, Charlie, David]) // Bob in expanded so skip
    = [Alice, Charlie, David, Mary]
     */
    @tailrec
    def findPathTailrec(remaining: List[List[T]], expanded: Set[T]): List[T] = {
      if (remaining.isEmpty) List()
      else {
        val currentPath = remaining.head
        val currentEndPoint = currentPath.head

        // add the length check so that this impl support checking for cycles
        if (currentEndPoint == end && currentPath.length > 1)  currentPath.reverse
        else if (expanded.contains(currentEndPoint)) findPathTailrec(remaining.tail, expanded)
        else {
          findPathTailrec(
            remaining.tail ++ graph(currentEndPoint).toList.map(_ :: currentPath),
            expanded + currentEndPoint
          )
        }
      }
    }

    findPathTailrec(List(List(start)), Set())
  }

  def findCycle[T](graph: Graph[T], node: T): List[T] = findPath(graph, node, node)

  // my impl
  // this version works on finding all edges as a list of tuples, flipping each tuple, the folding over the
  // resulting list of edges to construct the map
  def makeUndirected[T](graph: Graph[T]): Graph[T] = {
//        graph.keys
//          .flatMap { k => graph(k).flatMap { n => List((k, n), (n, k)) } }
//          .foldLeft(Map[T, Set[T]]()) {
//            case (acc, (node, neighbor)) => acc + (node -> (acc.getOrElse(node, Set()) + neighbor))
//          }

    (for {
      node <- graph.keys
      neighbor <- graph(node)
      pairs <- List((node, neighbor), (neighbor, node))
    } yield pairs)
      .foldLeft(Map[T, Set[T]]()) {
        case (acc, (node, neighbor)) => acc + (node -> (acc.getOrElse(node, Set()) + neighbor))
      }
  }

  // Daniel's impl
  def makeUndirected_v2[T](graph: Graph[T]): Graph[T] = {
    // add an association of from -> to into the graph
    def addEdge(graph: Graph[T], from: T, to: T): Graph[T] = graph + (from -> (graph.getOrElse(from, Set()) + to))

    @tailrec
    def addOpposingEdges(remaining: Set[T], acc: Graph[T]): Graph[T] = {
      if (remaining.isEmpty) acc
      else {
        val node = remaining.head
        val neighbors = graph(node)
        val newGraph = neighbors.foldLeft(acc) {
          case (intAcc, neighbor) => addEdge(intAcc, neighbor, node)
        }

        addOpposingEdges(remaining.tail, newGraph)
      }
    }

    addOpposingEdges(graph.keySet, graph)
  }

  /**
    * Hard problems
    */

    // my impl
    def color[T](graph: Graph[T]): Map[T, Int] = {
      val numNodes = graph.keySet.size

      /*
      Start by picking node with most neighbors
      [Alice], [], {Alice -> [0 1 2], Bob -> [0 1 2], Charlie -> [0 1 2], David -> [0 1 2], Mary -> [0 1 2]}
      [Bob, Charlie, David], [Alice], {Alice -> [0], Bob -> [1 2], Charlie -> [1 2], David -> [1 2], Mary -> [0 1 2]}
      [Charlie, David], [Alice, Bob], {Alice -> [0], Bob -> [1 2], Charlie -> [1], David -> [2], Mary -> [0 1 2]}
      [David, David], [Alice, Bob, Charlie], {Alice -> [0], Bob -> [1], Charlie -> [1], David -> [2], Mary -> [0 1]}
      [Mary, Bob, David], [Alice, Bob, Charlie, David], {Alice -> [0], Bob -> [1], Charlie -> [1], David -> [2], Mary -> [0]}
      {Alice -> 0, Bob -> 1, Charlie -> 1, David -> 2, Mary -> 0}

      [Alice], [], {Alice -> [0 1 2], Bob -> [0 1 2], Charlie -> [0 1 2], David -> [0 1 2], Mary -> [0 1 2]}
      [David, Charlie, Bob], [Alice], {Alice -> [0], Bob -> [1 2], Charlie -> [1 2], David -> [1 2], Mary -> [0 1 2]}
      [Bob, Mary, Charlie, Bob], [Alice, David], {Alice -> [0], Bob -> [2], Charlie -> [1 2], David -> [1], Mary -> [0 2]}
      [Mary, Charlie, Bob], [Alice, David, Bob], {Alice -> [0], Bob -> [2], Charlie -> [1 2], David -> [1], Mary -> [0 2]}
      [Bob, Charlie, Charlie, Bob], [Alice, David, Bob, Mary], {Alice -> [0], Bob -> [2], Charlie -> [1 2], David -> [1], Mary -> [0]}
      [Charlie, Charlie, Bob], [Alice, David, Bob, Mary], {Alice -> [0], Bob -> [2], Charlie -> [1 2], David -> [1], Mary -> [0]}
      [David, Charlie, Bob], [Alice, David, Bob, Mary, Charlie], {Alice -> [0], Bob -> [2], Charlie -> [2], David -> [1], Mary -> [0]}
       */

      @tailrec
      def colorTailrec(remaining: List[T], visited: Set[T], acc: Map[T, List[Int]]): Map[T, Int] = {
        if (remaining.isEmpty) Map()
        else if (acc.values.flatten.size == numNodes) acc.map { case (node, colors) => (node -> colors.head) }
        else {
          val node = remaining.head
          if (visited.contains(node)) colorTailrec(remaining.tail, visited, acc)
          else {
            val newNodeColor = acc(node).head
            val neighbors = graph(node)
            val newAcc = acc ++
              Map(node -> List(newNodeColor)) ++
              neighbors.foldLeft(Map[T, List[Int]]()) {
                case (updatedAcc, neighbor) => updatedAcc + (neighbor -> acc(neighbor).filter(_ != newNodeColor))
              }

            colorTailrec(neighbors.toList ++ remaining.tail, visited + node, newAcc)
          }
        }
      }

      implicit val ordering: Ordering[(T, Set[T])] = Ordering.fromLessThan { (a,b) =>
        a._2.size <= b._2.size
      }
      val startingNode = graph.max._1

      val startingColors = graph.keySet.foldLeft(Map[T, List[Int]]()) {
        case (acc, node) => acc + (node -> List(0, 1, 2))
      }

      colorTailrec(List(startingNode), Set(), startingColors)
    }

  // Daniel impl
  def color_v2[T](graph: Graph[T]): Map[T, Int] = {
    val undirected = makeUndirected(graph)

    /*
    Make graph undirected
      Alice -> [Bob, Charlie, David]
      Bob -> [Alice, David, Mary]
      Charlie -> [David, Alice, Mary]
      David -> [Bob, Mary, Alice, Charlie]
      Mary -> [Bob, Charlie, David]

     Sort by outdegree then call colorTailrec

      ctr([David, Alice, Bob, Charlie, Mary], 0, {}) // add color to all nodes NOT connected to node
      ctr([Alice, Bob, Charlie, Mary], 1, { David -> 0 })
      ctr([Bob, Charlie, Mary], 2, { David -> 0, Alice -> 1, Mary -> 1 })
      ctr([Charlie, Mary], 0, { David -> 0, Alice -> 1, Mary -> 1, Bob -> 2, Charlie -> 2 })
      ctr([Mary], 0, { David -> 0, Alice -> 1, Mary -> 1, Bob -> 2, Charlie -> 2 })
      ctr([], 0, { David -> 0, Alice -> 1, Mary -> 1, Bob -> 2, Charlie -> 2 })
      { David -> 0, Alice -> 1, Mary -> 1, Bob -> 2, Charlie -> 2 })
     */
    @tailrec
    def colorTailrec(remaining: List[T], currentColor: Int, colorings: Map[T, Int]): Map[T, Int] = {
      if (remaining.isEmpty) colorings
      else {
       val node = remaining.head
       if (colorings.contains(node)) colorTailrec(remaining.tail, currentColor, colorings)
       else {
         val uncoloredNodes = remaining.tail.foldLeft[Set[T]](Set(node)) { case (nodesToBeColored, n) =>
           val allNeighbors = nodesToBeColored.flatMap(nodeToBeColored => undirected(nodeToBeColored))
           if (colorings.contains(n) || allNeighbors.contains(n)) nodesToBeColored
           else nodesToBeColored + n
         }

         val newColorings = uncoloredNodes.map((_, currentColor)).toMap
         colorTailrec(remaining.tail, currentColor + 1, colorings ++ newColorings)
       }
      }
    }


    val nodesOrdered = undirected.keySet.toList.sortWith((a,b) => outDegree(undirected, a) > outDegree(undirected, b))

    colorTailrec(nodesOrdered, 0, Map())
  }

  def testEasy(): Unit = {
    println(outDegree(socialNetwork, "Alice")) //3
    println(inDegree(socialNetwork, "David")) //2
  }
//  testEasy()

  def testMedium(): Unit = {
    println(isPath(socialNetwork, "Alice", "Mary")) // true
    println(isPath(socialNetwork, "Mary", "Alice")) // false

    println(findPath(socialNetwork, "Alice", "Mary"))
    println(findPath(socialNetwork, "Mary", "Alice"))
    println(findPath(socialNetwork, "Alice", "Bob"))

    println(findCycle(socialNetwork, "Alice"))
    println(findCycle(socialNetwork, "Charlie"))

    println(makeUndirected(socialNetwork))
    println(makeUndirected_v2(socialNetwork))
  }
//  testMedium()

    def testHard(): Unit = {
      println(color(socialNetwork))
      println(color_v2(socialNetwork))
    }
      testHard()


}
