package org.igye.logic.graph.queryengine

import org.igye.logic.{Predicate, Substitution}

case class RuleTail(parent: RuleNode, collectedSubsts: Substitution, query: Set[Predicate], orderNumber: Int) extends RuleNode