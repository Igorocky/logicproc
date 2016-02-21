package org.igye.logic.graph.transfengine

import org.igye.logic.{Predicate, SubRule, Substitution}

case class PossibleTransformation(part: Predicate, rule: SubRule, eqLeft: Predicate, subs: Substitution)