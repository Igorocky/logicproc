package org.igye.logic

case class SubRule(conjSet: Set[Predicate], result: Predicate, parent: Rule) {
    override def toString: String = s"$conjSet --> $result"
}