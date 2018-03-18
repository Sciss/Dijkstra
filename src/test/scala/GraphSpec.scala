import org.seaton.dijkstra.core._
import org.specs2.mutable.Specification

class GraphSpec extends Specification {

  val triNodes		: Map[String, Node[String]] = Map("a" -> Node("a", 0.0, 0.0), "b" -> Node("b", 100.0, 100.0), "c" -> Node("c", 100.0, 0.0))
  val goodTriEdges: List[Edge[String]] 				= List(Edge("a", "b"), Edge("b", "c"), Edge("c", "a"))
  val badTriEdges	: List[Edge[String]] 				= Edge("d", "e") :: goodTriEdges

  "graph instantiation should fail for edges with invalid node ids" in {
    Graph(triNodes, badTriEdges) must throwA[RuntimeException] // try to create graph with bad node ids in edges
  }
}