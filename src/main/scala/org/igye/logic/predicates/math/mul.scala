package org.igye.logic.predicates.math

import org.igye.logic.Predicate
import org.igye.logic.predicates.ObjectCache

case class mul(left: Predicate, right: Predicate) extends Predicate(left, right) {
    override def copy(orderedChildren: List[Predicate]): mul = mul(orderedChildren(0), orderedChildren(1))
}

object mulObj extends ObjectCache[mul] {
    def apply(left: Predicate, right: Predicate) = {
        cache(new mul(left, right))
    }
}