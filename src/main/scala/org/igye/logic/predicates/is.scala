package org.igye.logic.predicates

import org.igye.logic.Predicate

case class Is(left: Predicate, right: Predicate) extends Predicate(left, right) {

    override def copy(orderedChildren: List[Predicate]): Is = Is(orderedChildren(0), orderedChildren(1))

    override def toString: String = s"$left is $right"
}