package org.igye.logic.graph

import org.igye.logic.Predicate

case class RootNode(query: Predicate, orderNumber: Int) extends Node {
    override val parent: Node = null
}