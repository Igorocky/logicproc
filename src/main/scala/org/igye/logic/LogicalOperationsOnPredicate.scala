package org.igye.logic

import org.igye.logic.LogicOperators.{!, &, or}
import org.igye.logic.predicates.common._
import org.igye.logic.predicates.math.{add, mul}

class LogicalOperationsOnPredicate(base: Predicate) {
    def &(a: Predicate): & = new &(base, a)
    def or(a: Predicate): or = new or(base, a)
    def unary_! : ! = new !(base)

    def -->(result: Predicate): Rule = Rule(base, result)
    def ==>(a: Predicate): eqTo = eqTo(base, a)
    def <=>(a: Predicate): eqToBid = eqToBid(base, a)

    def is(a: Predicate): is = new is(base, a)
    def of(a: Predicate): of = new of(base, a)
    def belongsTo(a: Predicate): belongsTo = new belongsTo(base, a)

    def add(a: Predicate): add = new add(base, a)
    def mul(a: Predicate): mul = new mul(base, a)
}


object LogicalOperationsOnPredicate {
    implicit def predicateToLogicalOperationsOnPredicate(a: Predicate): LogicalOperationsOnPredicate =
        new LogicalOperationsOnPredicate(a)
}