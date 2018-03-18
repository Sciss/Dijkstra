package de.sciss.dijkstra

import java.lang.StringBuilder

import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait GraphBase[S] {
  /** Calculates via folding the distance traversed by a list of connected nodes.
    *
    * @param graph      the graph to be traverse
    * @param traversed  the list of node ids to be traversed
    *
    * @return the distance travelled by traversing the nodes
    */
  def traversedDistance(graph: Graph[S], traversed: List[S]): Double = {
    var prev = Option.empty[S]
    traversed.foldLeft(0.0)((acc, nid) => prev match {
      case None =>
        prev = Some(nid)
        0.0
      case Some(pv) =>
        prev = Some(nid)
        acc + graph.net(nid)(pv)
    })
  }

  /** Calculates the shortest path given a graph, source and target nodes, and neighbor and distance functions.
    *
    * @param net       graph represented by nodes with connected nodes and distances
    * @param source    source node id in graph
    * @param target    target/destination node id in graph
    * @param neighbors function to determine neighbors to a particular node
    *
    * @return traversable list of node ids in graph representing the shortest path
    */
  def shortestPath(net: Map[S, Map[S, Double]], source: S, target: S,
    neighbors: ((Map[S, Map[S, Double]], S)    => List[S])        = this.neighbors,
    distance : ((Map[S, Map[S, Double]], S, S) => Option[Double]) = this.distance): GraphCase[S] = {

    if (source == target) {
      ShortestRoute(List(target), 0.0)
    } else if (!net.contains(source) || !net.contains(target)) {
      ShortestRouteInvalidSourceOrTarget()
    } else {
      val predMap = dijkstra(net, source, target, neighbors, distance)
      buildPath(predMap, source, target) match {
        case Some(p)  => ShortestRoute(p, predMap(target)._2) // XXX TODO should not call `Map#apply`
        case _        => ShortestRouteDoesNotExist()
      }
    }
  }

  /** Calculates the shortest path given a graph, source and target nodes, and neighbor and distance functions.
    *
    * @param graph graph represented by nodes and edges
    * @param source source node id in graph
    * @param target target/destination node id in graph
    *
    * @return traversable list of node ids in graph representing the shortest path
    */
  def shortestPath(graph: Graph[S], source: S, target: S): GraphCase[S] =
    shortestPath(graph.net, source, target)

  /** First-order function to determine connected nodes to given node in graph. */
  val neighbors: (Map[S, Map[S, Double]], S) => List[S] = { (netMap, nid) =>
    netMap.getOrElse(nid, Map.empty).keysIterator.toList
  }

  /** First-order function to determine distance between nodes with (x,y) coordinates. */
  val distance: (Map[S, Map[S, Double]], S, S) => Option[Double] = { (netMap, source, target) =>
    netMap.get(source).flatMap(_.get(target))
  }

  /** Determines minimum distance node.
    *
    * @param rDistances   sorted map with distance as key and edges with those distances
    * @param predMap      map with node ids pointing to previous distances to other nodes
    *
    * @return Tuple(minimum distance node id, minimum distance, updated relative distances, update previous distances)
    */
  private def takeMinNode(rDistances: SortedMap[Double, Map[S, S]],
                          predMap: Map[S, (S, Double)]): (S, Double, SortedMap[Double, Map[S, S]], Map[S, (S, Double)]) = {

    val dist        = rDistances.firstKey
    val minNodes    = rDistances(dist)
    val minNode     = minNodes.head._1
    val prevNid     = minNodes.head._2
    val otherNodes  = minNodes.tail
    val rMapOut     =
      if (otherNodes.isEmpty) rDistances - dist
      else                    rDistances + (dist -> otherNodes)

    val predMapOut  = predMap + (minNode -> ((prevNid, dist)))
    (minNode, dist, rMapOut, predMapOut)
  }

  /** Adds relative distances.
    *
    * @param rDistances sorted map with distance as key and edges corresponding to distance.
    * @param nid        node id
    * @param prevNid    previous node id
    * @param dist       distance
    * @param prevDist   previous distance
    *
    * @return updated relative distance sorted map
    */
  private def addRelDist(rDistances: SortedMap[Double, Map[S, S]], nid: S, prevNid: S, dist: Double,
                         prevDist: Double = -1.0): SortedMap[Double, Map[S, S]] = {

    if (prevDist < 0) {
      rDistances.get(dist) match {
        case Some(nodes)  => rDistances + (dist -> (nodes + (nid -> prevNid)))
        case _            => rDistances + (dist -> Map(nid -> prevNid))
      }
    } else {
      val nrDistances = addRelDist(rDistances, nid, prevNid, dist)
      val minNodes    = rDistances(prevDist)
      val nMinNodes   = minNodes - nid
      if (nMinNodes.isEmpty) {
        nrDistances - prevDist
      } else {
        nrDistances + (dist -> Map(nid -> prevNid))
      }
    }
  }

  /** Recursive function to build path from distance and previous node/distance maps.
    *
    * @param pDist   tuple with node id and distance
    * @param predMap map with node id as key and tuple of connected node id and distance
    * @param source  source node id
    * @param path    list of node ids
    *
    * @return `Option[List[String]]` with shortest (least cost) path node ids
    */
  private def buildPathRecur(pDist: (S, Double), predMap: Map[S, (S, Double)],
                             source: S, path: List[S]): Option[List[S]] = {
    val pred = pDist._1
    if (pred == source) {
      Some(source :: path)
    } else {
      predMap.get(pred) match {
        case Some(tup)  => buildPathRecur(tup, predMap, source, pred :: path)
        case None       => None
      }
    }
  }

  /** Initialization build path function to calls the recursive build path function to build path from
    *  distance and previous node/distance maps.
    *
    * @param predMap  map with node id as key and tuple of connected node id and distance
    * @param source   source node id
    * @param target   target node id
    *
    * @return `Option[List[String]]` with shortest (least cost) path node ids
    */
  private def buildPath(predMap: Map[S, (S, Double)], source: S, target: S): Option[List[S]] =
    predMap.get(target).flatMap(tup => buildPathRecur(tup, predMap, source, List(target)))
  
  /** Updates the relative distances between nodes.
    *
    * @param rDistances sorted map with distance as key and edges with those distances
    * @param net        map with node id as key and tuples with connected nodes and distance
    * @param nid        current node id to update
    * @param dist       distance
    * @param neighbors  function to determine connected nodes (edges)
    * @param distance   function to determine distance between nodes
    *
    * @return updated relative distances and previous distances.
    */
  private def updateRelDistances(
      rDistances: SortedMap[Double, Map[S, S]],
      predMap   : Map[S, (S, Double)],
      net       : Map[S, Map[S, Double]],
      nid       : S,
      dist      : Double,
      neighbors : ((Map[S, Map[S, Double]], S) => List[S]),
      distance  : ((Map[S, Map[S, Double]], S, S) => Option[Double])): (SortedMap[Double, Map[S, S]], Map[S, (S, Double)]) = {

    val chds = neighbors(net, nid)
    chds.foldLeft((rDistances, predMap)) { (rPair, neighbor) =>
      val curDist: Double = distance(net, nid, neighbor) match {
        case Some(db) => db + dist
        case _        => -1.0
      }

      val prevDist: Double = predMap.get(neighbor) match {
        case Some(pPair)  => pPair._2.asInstanceOf[Double]
        case _            => -1.0
      }
      val nrDistances = rPair._1
      val nPredMap    = rPair._2
      if (prevDist == -1.0) {
        val ard = addRelDist(nrDistances, neighbor, nid, curDist)
        (ard, nPredMap + (neighbor -> ((nid, curDist))))
        
      } else {
        if (curDist < prevDist) {
          val ard = addRelDist(nrDistances, neighbor, nid, curDist, prevDist)
          (ard, nPredMap + (neighbor -> ((nid, curDist))))
        } else {
          rPair
        }
      }
    }
  }

  /** Recursively calculates the shortest route from root to destination node.
    *
    * @param net        graph represented by nodes with connected nodes and distances
    * @param source     starting node id
    * @param target     ending node id
    * @param neighbors  function to determine neighbors to a particular node
    * @param distance   function to calculate/retrieve distance between two connection nodes in graph
    * @param rDistances sorted map with distance as key and edges with those distances
    * @param minNode    closest node id
    * @param predMap    map with node ids pointing to previous distances to other nodes
    * @param dist       current distance between nodes
    *
    * @return updated previous distances
    */
  private def short(net: Map[S, Map[S, Double]], source: S, target: S,
      neighbors : ((Map[S, Map[S, Double]], S)    => List[S]),
      distance  : ((Map[S, Map[S, Double]], S, S) => Option[Double]),
      rDistances: SortedMap[Double, Map[S, S]],
      minNode   : S,
      predMap   : Map[S, (S, Double)],
      dist      : Double): Map[S, (S, Double)] = {

    if (rDistances.isEmpty) {
      predMap
    } else {
      val take    = takeMinNode(rDistances, predMap)
      val update  = updateRelDistances(take._3, take._4, net, take._1, take._2, neighbors, distance)
      short(net, source, target, neighbors, distance, update._1, take._1, update._2, take._2)
    }
  }

  /** Calculates the shortest path between connected nodes.
    *
    * @param netMap     graph represented by nodes with connected nodes and distances
    * @param source     source node id in graph
    * @param target     target/destination node id in graph
    * @param neighbors  function to determine neighbors to a particular node
    * @param distance   function to calculate/retrieve distance between two connection nodes in graph
    *
    * @return map with node id as key and relative distances to each connected node
    */
  private def dijkstra(netMap: Map[S, Map[S, Double]], source: S, target: S,
      neighbors : ((Map[S, Map[S, Double]], S)    => List[S]),
      distance  : ((Map[S, Map[S, Double]], S, S) => Option[Double])): Map[S, (S, Double)] = {

    val rDistances  = SortedMap(0.0 -> Map(source -> source))
    val minNode     = source
    val predMap     = Map(source -> ((source, 0.0)))
    val dist        = 0.0
    short(netMap, source, target, neighbors, distance, rDistances, minNode, predMap, dist)
  }
}

/** Represents a graph with a map of nodes and a list of edges.
  *
  * @param nodes map of graph nodes with node id as key
  * @param edges list of graph edges
  */
case class Graph[S](nodes: Map[S, Node[S]], edges: List[Edge[S]]) extends GraphBase[S] {

  // determine bad edge node ids (edge node ids that don't exist in nodes map)
  private val badEdgeNodeIds = new ListBuffer[S]()
  edges.foreach { edge =>
    if (!nodes.contains(edge.nodeA)) badEdgeNodeIds += edge.nodeA
    if (!nodes.contains(edge.nodeB)) badEdgeNodeIds += edge.nodeB
  }
  if (badEdgeNodeIds.nonEmpty) throw new RuntimeException("invalid node ids in edges: " + badEdgeNodeIds.toList)

  /** Graph as nodes and node distances. */
  lazy val net: Map[S, Map[S, Double]] = {
    val nDist = new mutable.HashMap[S, Map[S, Double]]()
    nodes.foreach { node =>
      val n2n = new mutable.HashMap[S, Double]()
      val _neighbors = neighborsOf(node._1)
      _neighbors.foreach { nid =>
        val dist = distanceBetween(node._1, nid)
        n2n += nid -> dist
      }
      nDist += node._1 -> n2n.toMap
    }
    nDist.toMap
  }

  /** Pulls the neighbors from the graph as nodes and distances structure.
    *
    * @param nid source node to find neighbors of
    *
    * @return list of neighbor node ids
    */
  private def neighbors(nid: S): List[S] =
    net.getOrElse(nid, Map.empty).keysIterator.toList

  /** Pulls the distances between nodes from graph as nodes and distances structure.
    *
    * @param src  source node id (direction is not a factor)
    * @param dest destination node id
    *
    * @return Option[Double] distance between src and dst nodes
    */
  private def distances(src: S, dest: S): Option[Double] =
    net.get(src).flatMap(_.get(dest))

  /** Determines the minimum distance node id.
    *
    * @param dist map of node distances with node id as key
    * @param work list of currently active node ids
    * @return node id of the node with least distance
    */
  private def minDistanceId(dist: mutable.HashMap[S, Double], work: ListBuffer[S]): Option[S] = {
    var min: Double     = Graph.INFINITE
    var mid: Option[S]  = None
    dist.foreach { case (id, d) =>
      if (work.contains(id)) {
        if (d < min) {
          min = d
          mid = Some(id)
        }
      }
    }
    mid
  }

  /** Determines the neighbors of a node.
    *
    * @param nid node id
    *
    * @return list of node ids of neighboring nodes (connected by edge)
    */
  private def neighborsOf(nid: S): List[S] =
    for (e <- edges; if e.nodeA.equals(nid) || e.nodeB.equals(nid))
      yield if (e.nodeA.equals(nid)) e.nodeB else e.nodeA

  /** Calculates the Cartesian distance between two (2) nodes (x,y).
    *
    * @param aid first node id
    * @param bid second node id
    *
    * @return distance (Cartesian) between first and second node
    */
  private def distanceBetween(aid: S, bid: S): Double =
    math.sqrt(math.pow(nodes(aid).x - nodes(bid).x, 2) + math.pow(nodes(aid).y - nodes(bid).y, 2))

  /** Calculates the shortest route (if any) between two (2) nodes in this graph.
    *
    * @param source (starting) node id
    * @param target (ending) node id
    */
  def shortestPath(source: S, target: S): GraphCase[S] = {
    if (source == target) {
      ShortestRoute(source :: Nil, 0.0)
    } else if (!nodes.contains(source) || !nodes.contains(target)) {
      ShortestRouteInvalidSourceOrTarget()
    } else {
      val distance = mutable.HashMap.empty[S, Double]
      val previous = mutable.HashMap.empty[S, S]
      val working = ListBuffer[S]()
      nodes.keysIterator.foreach { k =>
        distance += k -> Graph.INFINITE
        previous -= k
        working  += k
      }
      distance += source -> 0.0

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
              val neighM = neighbors(mid)
              neighM.foreach { neighbor =>
                distances(mid, neighbor) match {
                  case Some(dist) =>
                    val alternate = distance(mid) + dist
                    if (alternate < distance(neighbor)) {
                      distance(neighbor) = alternate
                      previous(neighbor) = mid
                    }
                  case _ => println("""distance calc failed for edge %s and %s""".format(closest, neighbor))
                }
              }
            }
          case _ => working.clear() // no more connected nodes to source
        }
      }
      if (closest.forall(distance(_) == Graph.INFINITE)) {
        ShortestRouteDoesNotExist()
      } else {
        val route     = new ListBuffer[S]()
        var location  = target
        while (previous.contains(location)) {
          route.insert(0, nodes(previous(location)).id)
          location = previous(location)
        }
        if (route.isEmpty) {
          ShortestRouteDoesNotExist()
        } else {
          route += target
          val routeL  = route.toList
          val tDist   = traversedDistance(this, routeL)
          ShortestRoute(routeL, tDist)
        }
      }
    }
  }

  /** Clones a new instance of the <code>Graph</code>.
    *
    * @return a new cloned instance of this <code>Graph</code>
    */
  override def clone(): Graph[S] = Graph[S](nodes, edges)

  /** Lazy string representation of the graph. */
  lazy val str: String = {
    val sb = new StringBuilder()
    sb.append("nodes: ")
    nodes foreach (nd => sb.append("\n" + nd._2.toString))
    sb.append("\nedges: ")
    edges foreach (edge => sb.append("\n" + edge.toString()))
    sb.toString
  }

  /** Overridden toString that returns the lazy val str. */
  override def toString: String = str

}

/** Companion object to Graph class with constants. */
object Graph {
  /** Represents an infinite distance while calculating distances between nodes. */
  val INFINITE: Double = Double.MaxValue
}