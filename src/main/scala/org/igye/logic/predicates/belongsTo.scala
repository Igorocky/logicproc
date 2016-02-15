package org.igye.logic.predicates

import org.igye.logic.Predicate

case class BelongsTo(left: Predicate, right: Predicate) extends Predicate(left, right) {

    override def copy(orderedChildren: List[Predicate]): BelongsTo = BelongsTo(orderedChildren(0), orderedChildren(1))

    override def toString: String = s"$left belongsTo $right"
}