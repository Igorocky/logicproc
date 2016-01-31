package org.igye.logic

case class Rule(condition: Predicate, result: Predicate) {
    override def toString: String = s"$condition ==> $result"
}