package org.igye.logic.predicates.common

import org.igye.logic.Predicate

case class eqTo(left: Predicate, right: Predicate) extends Predicate(left, right) {

    override def copy(orderedChildren: List[Predicate]): eqTo = eqTo(orderedChildren(0), orderedChildren(1))

    override def toString: String = s"$left ==> $right"
}