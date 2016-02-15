package org.igye.logic

import org.igye.logic.LogicalOperationsOnPredicate.predicateToLogicalOperationsOnPredicate
import org.igye.logic.predicates.Is

import scala.annotation.tailrec

object LogicOperators {
    case class !(a: Predicate) extends Predicate(a) {
        override def copy(orderedChildren: List[Predicate]): ! = new !(orderedChildren(0))

        override def toString: String = s"!($a)"
    }

    case class &(l: Predicate, r: Predicate) extends Predicate(l, r) {

        override def copy(orderedChildren: List[Predicate]): & = &(orderedChildren(0), orderedChildren(1))

        private def toStr(a: Predicate): String = a match {
            case r: or => s"($r)"
            case r: Is => s"($r)"
            case r => s"$r"
        }
        override def toString: String = s"${toStr(l)} & ${toStr(r)}"
    }

    case class or(l: Predicate, r: Predicate) extends Predicate(l, r) {

        override def copy(orderedChildren: List[Predicate]): or = or(orderedChildren(0), orderedChildren(1))

        override def toString: String = s"${l}  or  ${r}"
    }

    private def toDnfM(a: Any): Predicate = a match {
        case (l or r) & m =>
            val dnfForM = toDnfM(m)
            toDnfM(l) & dnfForM or toDnfM(r) & dnfForM
        case m & (l or r) =>
            val dnfForM = toDnfM(m)
                dnfForM & toDnfM(l) or dnfForM & toDnfM(r)
        case l & r => toDnfM(l) & toDnfM(r)
        case l or r => toDnfM(l) or toDnfM(r)
        case !(l & r) => !toDnfM(l) or !toDnfM(r)
        case !(l or r) => !toDnfM(l) & !toDnfM(r)
        case !(!(a)) => toDnfM(a)
        case !(a) => !toDnfM(a)
        case r: Predicate => r
    }

    @tailrec
    def toDnf(a: Predicate): Predicate = {
        val res = toDnfM(a)
        if (res == a) {
            res
        } else {
            toDnf(res)
        }
    }
}

