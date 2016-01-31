package org.igye.logic.predicates

import org.igye.logic.Predicate

case class is(who: Predicate, what: Predicate) extends Predicate(who, what) {

    override def copy(orderedChildren: List[Predicate]): is = is(orderedChildren(0), orderedChildren(1))

    override def toString: String = s"$who is $what"
}