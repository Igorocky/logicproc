package org.igye.logic.predicates.math

import org.igye.logic.Predicate

case class mul(left: Predicate, right: Predicate) extends Predicate(left, right) {
    override def copy(orderedChildren: List[Predicate]): mul = mul(orderedChildren(0), orderedChildren(1))
}