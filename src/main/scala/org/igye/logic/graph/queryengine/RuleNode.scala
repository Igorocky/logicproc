package org.igye.logic.graph.queryengine

import org.igye.logic.Predicate

trait RuleNode {
    val query: Set[Predicate]
}