package org.igye.logic.predicates.math

import org.igye.logic.Predicate

case class add(left: Predicate, right: Predicate) extends Predicate(left, right) {
    override def copy(orderedChildren: List[Predicate]): add = add(orderedChildren(0), orderedChildren(1))
}