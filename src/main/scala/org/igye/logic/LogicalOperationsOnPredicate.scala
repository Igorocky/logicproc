package org.igye.logic

import org.igye.logic.LogicOperators.{&, or, !}
import org.igye.logic.predicates._

class LogicalOperationsOnPredicate(base: Predicate) {
    def &(a: Predicate): & = new &(base, a)
    def or(a: Predicate): or = new or(base, a)
    def unary_! : ! = new !(base)

    def -->(result: Predicate): Rule = Rule(base, result)
    def is(a: Predicate): Is = Is(base, a)
    def of(a: Predicate): Of = Of(base, a)
    def belongsTo(a: Predicate): BelongsTo = BelongsTo(base, a)
    def ==>(a: Predicate): Equivalent = Equivalent(base, a)
    def <=>(a: Predicate): EquivalentBidirectional = EquivalentBidirectional(base, a)
}


object LogicalOperationsOnPredicate {
    implicit def predicateToLogicalOperationsOnPredicate(a: Predicate): LogicalOperationsOnPredicate =
        new LogicalOperationsOnPredicate(a)
}