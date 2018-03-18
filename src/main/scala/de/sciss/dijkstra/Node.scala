package de.sciss.dijkstra

/** Represents a node in a graph with Cartesian coordinates (x,y) and a string id.
  *
  * @param id node id
  * @param x X-coordinate of node
  * @param y Y-coordinate of node
  */
case class Node[+S](id: S, x: Double, y: Double) {
  /** Lazy string representation of Node. */
  lazy val str: String = s"$id: x=$x; y=$y"

  /**
    * Overridden toString() default string representation of Node.
    * @return lazy str representation
    */
  override def toString: String = str
}