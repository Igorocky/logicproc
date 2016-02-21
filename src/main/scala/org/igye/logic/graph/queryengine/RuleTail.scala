package org.igye.logic.graph.queryengine

import org.igye.logic.{SubRule, Predicate, Substitution}

case class RuleTail(rule: SubRule, collectedSubsts: Substitution, query: Set[Predicate]) extends RuleNode