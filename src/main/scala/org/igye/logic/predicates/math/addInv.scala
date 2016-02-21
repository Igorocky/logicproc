package org.igye.logic.predicates.math

import org.igye.logic.Predicate

case class addInv(arg: Predicate) extends Predicate(arg) {
  override def copy(orderedChildren: List[Predicate]): addInv = addInv(orderedChildren(0))
}