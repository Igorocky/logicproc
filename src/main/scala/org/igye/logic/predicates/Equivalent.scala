package org.igye.logic.predicates

import org.igye.logic.Predicate

case class Equivalent(left: Predicate, right: Predicate) extends Predicate(left, right) {

    override def copy(orderedChildren: List[Predicate]): Equivalent = Equivalent(orderedChildren(0), orderedChildren(1))

    override def toString: String = s"$left ==> $right"
}