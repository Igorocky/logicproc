package org.igye.logic.graph.queryengine

import org.igye.logic.LogicalExpressions.applySubstitution
import org.igye.logic.{Predicate, SubRule, Substitution}

case class RuleHead(collectedSubsts: Substitution,
                    rule: SubRule, gate: Substitution) extends RuleNode {
    override val query: Set[Predicate] = rule.conjSet.map(applySubstitution(_, collectedSubsts))
}