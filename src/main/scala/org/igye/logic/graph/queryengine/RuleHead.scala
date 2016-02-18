package org.igye.logic.graph.queryengine

import org.igye.logic.graph.common.Node
import org.igye.logic.{LogicalExpressions, Predicate, SubRule, Substitution}

case class RuleHead(parent: Node, collectedSubsts: Substitution,
                    rule: SubRule, gate: Substitution, orderNumber: Int) extends RuleNode {
    override val query: Set[Predicate] = rule.conjSet.map(LogicalExpressions.applySubstitution(_, collectedSubsts))
}