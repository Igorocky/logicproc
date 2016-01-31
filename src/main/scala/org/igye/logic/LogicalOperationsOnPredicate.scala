package org.igye.logic

import org.igye.logic.LogicOperators.{&, or, !}
import org.igye.logic.predicates.is

class LogicalOperationsOnPredicate(base: Predicate) {
    def &(a: Predicate): & = new &(base, a)
    def or(a: Predicate): or = new or(base, a)
    def unary_! : ! = new !(base)

    def is(a: Predicate): is = new is(base, a)
    def ==>(result: Predicate): Rule = Rule(base, result)
}


object LogicalOperationsOnPredicate {
    implicit def predicateToLogicalOperationsOnPredicate(a: Predicate): LogicalOperationsOnPredicate =
        new LogicalOperationsOnPredicate(a)
}