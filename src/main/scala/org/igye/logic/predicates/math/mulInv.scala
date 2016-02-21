package org.igye.logic.predicates.math

import org.igye.logic.Predicate

case class mulInv(arg: Predicate) extends Predicate(arg) {
    override def copy(orderedChildren: List[Predicate]): mulInv = mulInv(orderedChildren(0))
}