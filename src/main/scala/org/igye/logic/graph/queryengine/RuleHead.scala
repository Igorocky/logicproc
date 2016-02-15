package org.igye.logic.graph.queryengine

import org.igye.logic.graph.common.Node
import org.igye.logic.{Predicate, SubRule, Substitution}

case class RuleHead(parent: Node, rule: SubRule, gate: Substitution, orderNumber: Int) extends RuleNode {
    override val query: Set[Predicate] = rule.conjSet
}