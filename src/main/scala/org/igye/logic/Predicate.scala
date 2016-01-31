package org.igye.logic

abstract class Predicate(val orderedChildren: List[Predicate]) {
    def this(c1: Predicate, ci: Predicate*) = this(List(c1):::ci.toList)
    def copy(orderedChildren: List[Predicate]): Predicate
}
