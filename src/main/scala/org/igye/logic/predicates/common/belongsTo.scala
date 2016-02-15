package org.igye.logic.predicates.common

import org.igye.logic.Predicate

case class belongsTo(left: Predicate, right: Predicate) extends Predicate(left, right) {

    override def copy(orderedChildren: List[Predicate]): belongsTo = belongsTo(orderedChildren(0), orderedChildren(1))

    override def toString: String = s"$left belongsTo $right"
}