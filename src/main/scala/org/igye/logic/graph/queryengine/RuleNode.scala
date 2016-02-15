package org.igye.logic.graph.queryengine

import org.igye.logic.Predicate
import org.igye.logic.graph.common.Node

trait RuleNode extends Node {
    val query: Set[Predicate]
}