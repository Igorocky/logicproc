package org.igye.logic.predicates

import org.igye.logic.Predicate

object PredicateUtils {
  def calcDepth(pr: Predicate): Int = {
    if (pr.orderedChildren.isEmpty) {
      1
    } else {
      1 + pr.orderedChildren.map(calcDepth).max
    }
  }
}
