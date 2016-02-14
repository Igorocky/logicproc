package org.igye.logic.graph

import org.igye.logic.{Substitution, Predicate}

case class RuleTail(parent: RuleNode, collectedSubsts: Substitution, query: Set[Predicate], orderNumber: Int) extends RuleNode