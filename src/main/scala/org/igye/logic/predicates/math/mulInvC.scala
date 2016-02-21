package org.igye.logic.predicates.math

import org.igye.logic.Predicate
import org.igye.logic.predicates.ObjectCache

case class mulInvC(arg: Predicate) extends Predicate(arg) {
    override def copy(orderedChildren: List[Predicate]): mulInvC = mulInv(orderedChildren(0))
}

object mulInv extends ObjectCache[mulInvC] {
    def apply(arg: Predicate) = {
        cache(new mulInvC(arg))
    }
}