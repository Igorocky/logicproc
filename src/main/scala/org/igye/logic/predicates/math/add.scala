package org.igye.logic.predicates.math

import org.igye.logic.Predicate
import org.igye.logic.predicates.ObjectCache

case class add(left: Predicate, right: Predicate) extends Predicate(left, right) {
  override def copy(orderedChildren: List[Predicate]): add = add(orderedChildren(0), orderedChildren(1))
}

object addObj extends ObjectCache[add] {
  def apply(left: Predicate, right: Predicate) = {
    cache(new add(left, right))
  }
}