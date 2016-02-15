package org.igye.logic.predicates

import org.igye.logic.Predicate

case class eqToBid(left: Predicate, right: Predicate) extends Predicate(left, right) {

    override def copy(orderedChildren: List[Predicate]): eqToBid =
        eqToBid(orderedChildren(0), orderedChildren(1))

    override def toString: String = s"$left <=> $right"
}