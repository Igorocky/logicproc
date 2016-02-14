package org.igye.logic.graph

import org.igye.logic.Predicate

trait RuleNode extends Node {
    val query: Set[Predicate]
}