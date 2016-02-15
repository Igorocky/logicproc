package org.igye.logic.predicates

import org.igye.logic.Predicate

case class EquivalentBidirectional(left: Predicate, right: Predicate) extends Predicate(left, right) {

    override def copy(orderedChildren: List[Predicate]): EquivalentBidirectional =
        EquivalentBidirectional(orderedChildren(0), orderedChildren(1))

    override def toString: String = s"$left <=> $right"
}