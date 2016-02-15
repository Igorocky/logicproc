package org.igye.logic.graph.transfengine

import org.igye.logic.graph.common.Node
import org.igye.logic.{SubRule, Substitution, Rule, Predicate}

case class PossibleTransformation(parent: TransfResult, part: Predicate, rule: SubRule, eqLeft: Predicate,
                                  subs: Substitution, orderNumber: Int) extends Node {
    override def equals(other: Any): Boolean = {
        if (other == null) {
            false
        } else {
            other match {
                case PossibleTransformation(parent, part, rule, eqLeft, subs, _) =>
                    this.parent == parent && this.part == part && this.rule == rule
                case _ => false
            }
        }
    }
}