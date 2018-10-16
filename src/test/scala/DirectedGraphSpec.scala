import de.sciss.dijkstra.{Graph, GraphCase, Node, ShortestRoute}
import org.specs2.mutable.Specification

class DirectedGraphSpec extends Specification {
  final class Node1(override val id: Char)
    extends Node[Char](id, 0.0, 0.0)

  "graph shortest route should succeed with directed graph" in {
    // cf. https://stackoverflow.com/questions/13249057/dijkstra-find-shortest-path-in-directed-graph
    val graph0 = Map[Char, Set[(Int, Char)]](
      'A' -> Set(1 -> 'C'),
      'B' -> Set(3 -> 'S', 4 -> 'D'),
      'C' -> Set(3 -> 'D', 1 -> 'E'),
      'D' -> Set(1 -> 'E', 5 -> 'F', 3 -> 'T'),
      'E' -> Set(2 -> 'G', 4 -> 'T'),
      'F' -> Set(),
      'G' -> Set(2 -> 'E', 3 -> 'T'),
      'S' -> Set(4 -> 'A', 3 -> 'B', 7 -> 'D'),
      'T' -> Set()
    )

    val nodes = graph0.keysIterator.map(id => id -> new Node1(id)).toMap

    val g: Graph[Char] = new Graph[Char](nodes, Nil) {
      override lazy val net: Map[Char, Map[Char, Double]] = {
        graph0.map {
          case (id, targets) =>
            val targetMap = targets.iterator.map {
              case (cost, targetId) =>
                targetId -> cost.toDouble
            } .toMap
            id -> targetMap
        }
      }
    }

    val res: GraphCase[Char] = g.shortestPath('S', 'T')

    res must beOneOf (
      ShortestRoute("SBDT"  .toList, 10.0),
      ShortestRoute("SDT"   .toList, 10.0),
      ShortestRoute("SACET" .toList, 10.0)
    )
  }
}
