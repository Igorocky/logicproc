package org.igye.logic.predicates.common

import org.igye.logic.Predicate

case class is(left: Predicate, right: Predicate) extends Predicate(left, right) {

    override def copy(orderedChildren: List[Predicate]): is = is(orderedChildren(0), orderedChildren(1))

    override def toString: String = s"$left is $right"
}