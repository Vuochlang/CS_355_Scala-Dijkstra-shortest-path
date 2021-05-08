import scala.collection.mutable.ListBuffer
import scala.collection.mutable

class Dijkstra(m: Map[String, directedVertex]) {
  val myMap: Map[String, directedVertex] = m

  def successors(from: String): List[(String, String, Int)] = {
    val paths = myMap.get(from)
    var listToReturn = List[(String, String, Int)]()
    paths match {
      case None =>
      case Some(_) =>
        listToReturn = paths.get.to.map{case (key, value) => (from, key, value)}.toList  // (from, to, cost)
    }
    listToReturn
  }

  def minCost(l: ListBuffer[Node]): ListBuffer[Node] = {
    val myList = l.sortBy(r => r.total)
    myList
  }

  def unvisitedNodes(from: String): ListBuffer[Node] = {
    val s = successors(from).map(t => t._2) // get all adjacent paths of 'from'
    val keys = myMap.keys.filter(k => !s.contains(k) && k != from)  // all paths that is not adjacent to 'from'
    val listReturn = ListBuffer[Node]()
    keys.foreach(x => listReturn += Node(Int.MaxValue, x, None))
    listReturn
  }

  def getNode(s: String, list: ListBuffer[Node]): ListBuffer[Node]= {  // get a node based on 's' = path/state
      list.filter(n => n.state == s)
  }

  /**
   * The algorithm is as follows:
        1. Choose the start node and consider it as the current node
        2. Repeat the following steps until the current node is the end node
            2.1. Find all the accessible nodes from the current node
            2.2. Calculate the distance to access unvisited nodes
            2.3. Mark the current node as visited
            2.4. Choose the node with less distance
        3.Print the shortest path
   */

  def shortestPath(from: String, goal: String): Unit = {
    if (!myMap.contains(from)) {
      println(f"The starting path $from%s does not exist.")
      return
    }

    var activeList = ListBuffer[Node]() // map to get the min cost
    val visitedPath = mutable.Map[String, Node]()
    val currentNode = Node(0, from, None) // start node
    val d = mutable.Map[String, Int]()

    // adjacent path to the given 'from' : Node(7, "E", Node(0, "Home", None))
    myMap(from).to.foreach(m => activeList += Node(m._2, m._1, Some(currentNode)))
    myMap(from).to.foreach(m => d += (m._1 -> m._2))
    unvisitedNodes(from).foreach(node => activeList += node) // others path : Node(INT_MAX, "J", None)
    unvisitedNodes(from).foreach(node => d += (node.state -> node.total))
    visitedPath += (from -> currentNode)
    myMap(from).to.foreach(m => visitedPath += (m._1 -> Node(m._2, m._1, Some(currentNode))))

    while (activeList.nonEmpty) {
      val resultList = minCost(activeList)
      val shortestPath = resultList.head  // path with min cost
      activeList = resultList.tail

      for (each <- successors(shortestPath.state)) {
        if (each._2 == goal) {
          val newNode = Node(d(shortestPath.state) + each._3, each._2, Some(shortestPath))
          showPath(newNode)
          return
        }

        val temp = getNode(each._2, activeList)
        if (d(shortestPath.state) + each._3 < d(each._2)) { // d["D"] + C("F") < d["F"]
          d(each._2) = d(shortestPath.state) + each._3
          activeList -= temp.head

          visitedPath += (each._2 -> Node(d(each._2), each._2, Some(shortestPath)))
          activeList += Node(d(each._2), each._2, Some(shortestPath))
        }
      }
    }

  }

  def showPath(node: Node): Unit = {
    node.parent match {
      case None =>
        print(f"${node.state}%s ")
        return
      case Some(n) =>
        showPath(n)
    }
    print(f"${node.state}%s ")
  }
}

// (total = 0, current state = "D" Parent = Node(0, "Home", None))
case class Node(total: Int, s: String, parent: Option[Node]) {
  def totalCost: Int = total
  def state: String = s
  def parentState: Node = parent.get
}

case class directedVertex(start: String, goals: List[(String, Int)]) {
  def from: String = start
  def to: Map[String, Int] = goals.toMap
}

object Main {
  def main(args: Array[String]): Unit = {
    val graph = mutable.Map[String, directedVertex]()

    graph += ("Home" -> directedVertex("Home", List(("C", 1), ("D", 0), ("E", 7))))
    graph += ("C" -> directedVertex("C", List(("G", 11))))
    graph += ("D" -> directedVertex("D", List(("G", 5))))
    graph += ("E" -> directedVertex("E", List(("G", 3), ("F", 5))))
    graph += ("F" -> directedVertex("F", List(("H", 7), ("I", 10))))
    graph += ("G" -> directedVertex("G", List(("J", 1))))
    graph += ("H" -> directedVertex("H", List(("J", 0))))
    graph += ("I" -> directedVertex("I", List(("K", 2))))
    graph += ("J" -> directedVertex("J", List(("Work", 25))))
    graph += ("K" -> directedVertex("K", List(("Work", 25))))

    val dijkstra = new Dijkstra(graph.toMap)
    dijkstra.shortestPath("Home", "Work")

  }
}
