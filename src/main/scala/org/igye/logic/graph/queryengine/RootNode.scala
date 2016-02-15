package org.igye.logic.graph.queryengine

import org.igye.logic.Predicate
import org.igye.logic.graph.common.Node

case class RootNode(query: Predicate, orderNumber: Int) extends Node {
    override val parent: Node = null
}