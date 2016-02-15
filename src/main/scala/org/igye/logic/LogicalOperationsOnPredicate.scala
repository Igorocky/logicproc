package org.igye.logic

import org.igye.logic.LogicOperators.{&, or, !}
import org.igye.logic.predicates.{belongsTo, of, is}

class LogicalOperationsOnPredicate(base: Predicate) {
    def &(a: Predicate): & = new &(base, a)
    def or(a: Predicate): or = new or(base, a)
    def unary_! : ! = new !(base)

    def -->(result: Predicate): Rule = Rule(base, result)
    def is(a: Predicate): is = new is(base, a)
    def of(a: Predicate): of = new of(base, a)
    def belongsTo(a: Predicate): belongsTo = new belongsTo(base, a)
}


object LogicalOperationsOnPredicate {
    implicit def predicateToLogicalOperationsOnPredicate(a: Predicate): LogicalOperationsOnPredicate =
        new LogicalOperationsOnPredicate(a)
}