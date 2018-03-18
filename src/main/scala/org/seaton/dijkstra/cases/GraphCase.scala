package org.seaton.dijkstra.cases

import org.seaton.dijkstra.core.Graph

/**
 * Base GraphCase class
 */
sealed abstract class GraphCase

/**
  * Case class wrapping a generated graph.
  */
case class GeneratedGraph[S >: Null <: AnyRef](graph: Graph[S]) extends GraphCase

/**
  * Case class for an error occurring during the generation of a graph.
  */
case class GeneratedGraphFailed(msg: String) extends GraphCase

/**
  * Case class for shortest route not existing.
  */
case class ShortestRouteDoesNotExist() extends GraphCase

/**
  * Case class for an invalid source or target node id.
  */
case class ShortestRouteInvalidSourceOrTarget() extends GraphCase

/**
  * Case class wrapping the shortest route between two (2) nodes in a graph.
  */
case class ShortestRoute[S >: Null <: AnyRef](route: List[S], dist: Double) extends GraphCase

/**
  * Case class for an error occurring during a shortest route calculation in a graph.
  */
case class ShortestRouteError() extends GraphCase
