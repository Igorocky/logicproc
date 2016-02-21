package org.igye.logic.predicates.math

import org.igye.logic.Predicate
import org.igye.logic.predicates.ObjectCache

case class addInvC(arg: Predicate) extends Predicate(arg) {
  override def copy(orderedChildren: List[Predicate]): addInvC = addInv(orderedChildren(0))
}

object addInv extends ObjectCache[addInvC] {
  def apply(arg: Predicate) = {
    cache(new addInvC(arg))
  }
}