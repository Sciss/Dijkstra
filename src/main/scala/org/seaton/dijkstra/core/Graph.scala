package org.seaton.dijkstra.core

import java.lang.StringBuilder

import org.seaton.dijkstra.cases._

import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait GraphBase[S >: Null <: AnyRef] {
  /**
   * Calculates via folding the distance traversed by a list of connected nodes.
   *
   * @param graph the graph to be traverse
   * @param traversed the list of node ids to be traversed
   *
   * @return `Option[Double]` is the distance travelled by traversing the nodes
   */
  def traversedDistance(graph: Graph[S], traversed: List[S]): Double = {

    var prev: Option[S] = None
    traversed.foldLeft(0.0)((acc, nid) => prev match {
      case None =>
        prev = Some(nid)
        0.0
      case Some(pv) =>
        prev = Some(nid)
        acc + graph.net(nid)(pv)
    })
  }
  
  /**
   * Calculates the shortest path given a graph, source and target nodes, and neighbor and distance functions.
   *
   * @param net       graph represented by nodes with connected nodes and distances
   * @param source    source node id in graph
   * @param target    target/destination node id in graph
   * @param neighbors function to determine neighbors to a particular node
   *
   * @return `Option[List[String]]` traversable list of node ids in graph representing the shortest path
   */
  def shortestPath(net: Map[S, Map[S, Double]], source: S, target: S,
    neighbors: ((Map[S, Map[S, Double]], S) => List[S]) = neighbors,
    distance : ((Map[S, Map[S, Double]], S, S) => Option[Double]) = distance): GraphCase[S] = {

    if (source.equals(target)) {
      ShortestRoute(List(target), 0.0)
    } else if (!net.contains(source) || !net.contains(target)) {
      ShortestRouteInvalidSourceOrTarget()
    } else {
      val preds = dijkstra(net, source, target, neighbors, distance)
      buildPath(preds, source, target) match {
        case Some(p)  => ShortestRoute(p, preds(target)._2)
        case _        => ShortestRouteDoesNotExist()
      }
    }
  }

  /**
   * Calculates the shortest path given a graph, source and target nodes, and neighbor and distance functions.
   *
   * @param graph graph represented by nodes and edges
   * @param source source node id in graph
   * @param target target/destination node id in graph
   *
   * @return `Option[List[String]]` traversable list of node ids in graph representing the shortest path
   */
  def shortestPath(graph: Graph[S], source: S, target: S): GraphCase[S] =
    shortestPath(graph.net, source, target)

  /**
   * First-order function to determine connected nodes to given node in graph.
   */
  val neighbors: (Map[S, Map[S, Double]], S) => List[S] = { (net, nid) =>
    for (nbr <- net(nid).toList) yield nbr._1
  }

  /**
   * First-order function to determine distance between nodes with (x,y) coordinates.
   */
  val distance: (Map[S, Map[S, Double]], S, S) => Option[Double] = { (net, source, target) =>
    net(source).get(target)
  }

  /**
   * Determines minimum distance node.
   *
   * @param rdists  sorted map with distance as key and edges with those distances
   * @param preds   map with node ids pointing to previous distances to other nodes
   *
   * @return Tuple(minimum distance node id, minimum distance, updated relative distances, update previous distances)
   */
  private def takeMinNode(rdists: SortedMap[Double, Map[S, S]],
                          preds: Map[S, (S, Double)]): (S, Double, SortedMap[Double, Map[S, S]], Map[S, (S, Double)]) = {

    val dist        = rdists.firstKey
    val minNodes    = rdists(dist)
    val minNode     = minNodes.head._1
    val prevNid     = minNodes.head._2
    val otherNodes  = minNodes.tail
    (minNode, dist, if (otherNodes.isEmpty) rdists - dist else rdists + (dist -> otherNodes), preds + (minNode -> ((prevNid, dist))))
  }

  /**
   * Adds relative distances.
   *
   * @param rdists    sorted map with distance as key and edges corresponding to distance.
   * @param nid       node id
   * @param prevNid   previous node id
   * @param dist      distance
   * @param prevDist  previous distance
   *
   * @return `Option[]` with updated relative distance sorted map
   */
  private def addRdist(rdists: SortedMap[Double, Map[S, S]], nid: S, prevNid: S,
    dist: Double, prevDist: Double = -1.0): SortedMap[Double, Map[S, S]] = {

    if (prevDist < 0) {
      rdists.get(dist) match {
        case Some(nodes)  => rdists + (dist -> (nodes + (nid -> prevNid)))
        case _            => rdists + (dist -> Map(nid -> prevNid))
      }
    } else {
      val nrdists     = addRdist(rdists, nid, prevNid, dist)
      val minnodes  = rdists(prevDist)
      val nminnodes = minnodes - nid
      if (nminnodes.isEmpty) {
        nrdists - prevDist
      } else {
        nrdists + (dist -> Map(nid -> prevNid))
      }
    }
  }

  /**
   * Recursive function to build path from distance and previous node/distance maps.
   *
   * @param pdist   tuple with node id and distance
   * @param preds   map with node id as key and tuple of connected node id and distance
   * @param source  source node id
   * @param path    list of node ids
   *
   * @return `Option[List[String]]` with shortest (least cost) path node ids
   */
  private def buildPathRecur(pdist: (S, Double), preds: Map[S, (S, Double)], source: S, path: List[S]): Option[List[S]] = {
    val pred = pdist._1
    if (pred.equals(source)) {
      Some(source :: path)
    } else {
      preds.get(pred) match {
        case Some(tup)  => buildPathRecur(tup, preds, source, pred :: path)
        case None       => None
      }
    }
  }

  /**
   * Initialization build path function to calls the recursive build path function to build path from distance and previous node/distance maps.
   *
   * @param preds map with node id as key and tuple of connected node id and distance
   * @param source source node id
   * @param target target node id
   *
   * @return `Option[List[String]]` with shortest (least cost) path node ids
   */
  private def buildPath(preds: Map[S, (S, Double)], source: S, target: S): Option[List[S]] =
    preds.get(target).flatMap(tup => buildPathRecur(tup, preds, source, List(target)))
  
  /**
   * Updates the relative distances between nodes.
   *
   * @param rdists sorted map with distance as key and edges with those distances
   * @param net map with node id as key and tuples with connected nodes and distance
   * @param nid current node id to update
   * @param dist distance
   * @param neighbors function to determine connected nodes (edges)
   * @param distance function to determine distance between nodes
   *
   * @return Option[] with updated relative distances and previous distances.
   */
  private def updateRdists(rdists: SortedMap[Double, Map[S, S]],
      preds     : Map[S, (S, Double)],
      net       : Map[S, Map[S, Double]],
      nid       : S,
      dist      : Double,
      neighbors : ((Map[S, Map[S, Double]], S) => List[S]),
      distance  : ((Map[S, Map[S, Double]], S, S) => Option[Double])): (SortedMap[Double, Map[S, S]], Map[S, (S, Double)]) = {

    val chds = neighbors(net, nid)
    chds.foldLeft((rdists, preds)) { (rpair, neighbor) =>
      val curDist: Double = distance(net, nid, neighbor) match {
        case Some(db) => db + dist
        case _        => -1.0
      }

      val prevDist: Double = preds.get(neighbor) match {
        case Some(ppair) => ppair._2.asInstanceOf[Double]
        case _ => -1.0
      }
      val nrDists = rpair._1
      val nPreds  = rpair._2
      if (prevDist == -1.0) {
        val ard = addRdist(nrDists, neighbor, nid, curDist)
        (ard, nPreds + (neighbor -> ((nid, curDist))))
        
      } else {
        if (curDist < prevDist) {
          val ard = addRdist(nrDists, neighbor, nid, curDist, prevDist)
          (ard, nPreds + (neighbor -> ((nid, curDist))))
        } else {
          rpair
        }
      }
    }
  }

  /**
   * Recursively calculates the shortest route from root to destination node.
   *
   * @param net       graph represented by nodes with connected nodes and distances
   * @param source    starting node id
   * @param target    ending node id
   * @param neighbors function to determine neighbors to a particular node
   * @param distance  function to calculate/retrieve distance between two connection nodes in graph
   * @param rdists    sorted map with distance as key and edges with those distances
   * @param minNode   closest node id
   * @param preds     map with node ids pointing to previous distances to other nodes
   * @param dist      current distance between nodes
   *
   * @return `Option[]` update previous distances
   */
  private def short(net: Map[S, Map[S, Double]], source: S, target: S,
      neighbors : ((Map[S, Map[S, Double]], S)    => List[S]),
      distance  : ((Map[S, Map[S, Double]], S, S) => Option[Double]),
      rdists    : SortedMap[Double, Map[S, S]],
      minNode   : S,
      preds     : Map[S, (S, Double)],
      dist      : Double): Map[S, (S, Double)] = {

    if (rdists.isEmpty) {
      preds
    } else {
      val take    = takeMinNode(rdists, preds)
      val update  = updateRdists(take._3, take._4, net, take._1, take._2, neighbors, distance)
      short(net, source, target, neighbors, distance, update._1, take._1, update._2, take._2)
    }
  }

  /**
   * Calculates the shortest path between connected nodes.
   *
   * @param net graph represented by nodes with connected nodes and distances
   * @param source source node id in graph
   * @param target target/destination node id in graph
   * @param neighbors function to determine neighbors to a particular node
   * @param distance function to calculate/retrieve distance between two connection nodes in graph
   *
   * @return map with node id as key and relative distances to each connected node
   */
  private def dijkstra(net: Map[S, Map[S, Double]], source: S, target: S,
      neighbors : ((Map[S, Map[S, Double]], S)    => List[S]),
      distance  : ((Map[S, Map[S, Double]], S, S) => Option[Double])): Map[S, (S, Double)] = {

    val rdists  = SortedMap(0.0 -> Map(source -> source))
    val minNode = source
    val preds   = Map(source -> ((source, 0.0)))
    val dist    = 0.0
    short(net, source, target, neighbors, distance, rdists, minNode, preds, dist)
  }
}

/**
 * Represents a graph with a map of nodes and a list of edges.
 *
 * @param nodes map of graph nodes with node id as key
 * @param edges list of graph edges
 */
class Graph[S >: Null <: AnyRef](val nodes: Map[S, Node[S]], val edges: List[Edge[S]]) extends GraphBase[S] {

  // determine bad edge node ids (edge node ids that don't exist in nodes map)
  private val badEdgeNodeIds = new ListBuffer[S]()
  edges foreach (edge => {
    if (!nodes.contains(edge.nodeA)) badEdgeNodeIds += edge.nodeA
    if (!nodes.contains(edge.nodeB)) badEdgeNodeIds += edge.nodeB
  })
  if (badEdgeNodeIds.nonEmpty) throw new RuntimeException("invalid node ids in edges: " + badEdgeNodeIds.toList)

  /**
   * Graph as nodes and node distances.
   */
  lazy val net: Map[S, Map[S, Double]] = {
    val ndist = new mutable.HashMap[S, Map[S, Double]]()
    nodes foreach (node => {
      val n2n = new mutable.HashMap[S, Double]()
      val _neighbors = neighborsOf(node._1)
      _neighbors foreach (nid => {
        val dist = distanceBetween(node._1, nid)
        n2n += nid -> dist
      })
      ndist += node._1 -> n2n.toMap
    })
    ndist.toMap
  }

  /**
   * Pulls the neighbors from the graph as nodes and distances structure.
   *
   * @param nid source node to find neighbors of
   *
   * @return `Option[List[String]]` list of neighbor node ids
   */
  private def neighbors(nid: S): List[S] =
    for (nbr <- net(nid).toList) yield nbr._1

  /**
   * Pulls the distances between nodes from graph as nodes and distances structure.
   *
   * @param src source node id (direction is not a factor)
   * @param dest destination node id
   *
   * @return Option[Double] distance between src and dst nodes
   */
  private def distances(src: S, dest: S): Option[Double] =
    net(src).get(dest)

  /**
   * Determines the minimum distance node id.
   *
   * @param dist map of node distances with node id as key
   * @param work list of currently active node ids
   * @return node id of the node with least distance
   */
  private def minDistanceId(dist: mutable.HashMap[S, Double], work: ListBuffer[S]): Option[S] = {
    var min: Double = Graph.INFINITE
    var mid: Option[S] = None
    dist foreach (d => {
      if (work.contains(d._1)) {
        if (d._2 < min) {
          min = d._2
          mid = Some(d._1)
        }
      }
    })
    mid
  }

  /**
   * Determines the neighbors of a node.
   *
   * @param nid node id
   *
   * @return `Option[List[String]]` list of node ids of neighboring nodes (connected by edge)
   */
  private def neighborsOf(nid: S): List[S] =
    for (e <- edges; if e.nodeA.equals(nid) || e.nodeB.equals(nid)) yield if (e.nodeA.equals(nid)) e.nodeB else e.nodeA

  /**
   * Calculates the Cartesian distance between two (2) nodes (x,y).
   *
   * @param aid first node id
   * @param bid second node id
   *
   * @return Option[Double] distance (Cartesian) between first and second node
   */
  private def distanceBetween(aid: S, bid: S): Double =
    math.sqrt(math.pow(nodes(aid).x - nodes(bid).x, 2) + math.pow(nodes(aid).y - nodes(bid).y, 2))

  /**
   * Calculates the shortest route (if any) between two (2) nodes in this graph.
   *
   * @param source (starting) node id
   * @param target (ending) node id
   *
   * @return Option[ShortestRoute(List[Node])] for success, other graph cases for failed calculation
   */
  def shortestPath(source: S, target: S): GraphCase[S] = {
    if (source == target) {
      ShortestRoute(List(source), 0.0)
    } else if (!nodes.contains(source) || !nodes.contains(target)) {
      ShortestRouteInvalidSourceOrTarget()
    } else {
      val distance = mutable.HashMap.empty[S, Double]
      val previous = mutable.HashMap.empty[S, S]
      val working = ListBuffer[S]()
      nodes foreach (kv => {
        distance += (kv._1 -> Graph.INFINITE)
        previous -= kv._1
        working  += kv._1
      })
      distance += (source -> 0.0)

      var closest = Option.empty[S]
      while (working.nonEmpty) {
        minDistanceId(distance, working) match {
          case midS @ Some(mid) =>
            closest = midS
            if (distance(mid) == Graph.INFINITE) {
              println("no other nodes are accessible")
              closest = None
              working.clear()
            } else {
              working -= mid
              val neighbrs = neighbors(mid)
              neighbrs foreach (neighbor => {
                distances(mid, neighbor) match {
                  case Some(dist) =>
                    val alternate = distance(mid) + dist
                    if (alternate < distance(neighbor)) {
                      distance(neighbor) = alternate
                      previous(neighbor) = mid
                    }
                  case _ => println("""distance calc failed for edge %s and %s""".format(closest, neighbor))
                }
              })
            }
          case _ => working.clear() // no more connected nodes to source
        }
      }
      if (closest.forall(distance(_) == Graph.INFINITE)) {
        ShortestRouteDoesNotExist()
      } else {
        val route = ListBuffer[S]()
        var location = target
        while (previous.contains(location)) {
          route.insert(0, nodes(previous(location)).id)
          location = previous(location)
        }
        if (route.isEmpty) {
          ShortestRouteDoesNotExist()
        } else {
          route += target
          val tdist = traversedDistance(this, route.toList)
          ShortestRoute(route.toList, tdist)
        }
      }
    }
  }

  /**
   * Clones a new instance of the <code>Graph</code>.
   *
   * @return a new cloned instance of this <code>Graph</code>
   */
  override def clone(): Graph[S] = Graph[S](nodes, edges)

  /**
   * Lazy string representation of the graph.
   */
  lazy val str: String = {
    val sb = new StringBuilder()
    sb.append("nodes: ")
    nodes foreach (nd => sb.append("\n" + nd._2.toString))
    sb.append("\nedges: ")
    edges foreach (edge => sb.append("\n" + edge.toString()))
    sb.toString
  }

  /**
   * Overridden toString that returns the lazy val str.
   */
  override def toString: String = str

}

/**
 * Companion object to Graph class with simple factory apply(), constants, and functional Dijkstra's algorithm implementation.
 */
object Graph {

  /**
   * Factory for Graph class.
   *
   * @param nodes map of nodes in graph with node id as key
   * @param edges list of edges in graph
   *
   * @return new Graph instance
   */
  def apply[S >: Null <: AnyRef](nodes: Map[S, Node[S]], edges: List[Edge[S]]) = new Graph[S](nodes, edges)

  /**
   * Represents an infinite distance while calculating distances between nodes.
   */
  val INFINITE: Double = Double.MaxValue
}