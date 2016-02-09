package org.igye.logic.graph

trait Node extends Ordered[Node] {
    val parent: Option[Node]
}