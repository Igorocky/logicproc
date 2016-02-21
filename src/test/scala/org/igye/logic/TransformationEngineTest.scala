package org.igye.logic

import org.igye.logic.LogicalOperationsOnPredicate.predicateToLogicalOperationsOnPredicate
import org.igye.logic.predicates.math.addInv
import org.junit.Test

class TransformationEngineTest {
//    @Test
    def next(): Unit = {
        val R = StringPredicate("R")
        val _0 = StringPredicate("0")
        val a = StringPredicate("a")

        val x = Placeholder("x")
        val y = Placeholder("y")
        val z = Placeholder("z")

        val statements = new PredicateStorage(
            _0 belongsTo R
            ,a belongsTo R
        )

        val rules = new RuleStorage(
            {(x belongsTo R) & (y belongsTo R)} --> {(x add y) belongsTo R}
            ,{x belongsTo R} --> {addInv(x) belongsTo R}
            ,{(x belongsTo R) & (y belongsTo R)} --> {(x mul y) belongsTo R}

            ,/*1*/{x belongsTo R} --> {(x add _0) <=> x}
            ,/*2*/{x belongsTo R} --> {(x add addInv(x)) <=> _0}
            ,/*3*/{(x belongsTo R) & (y belongsTo R) & (z belongsTo R)} --> {(x add (y add z)) <=> ((x add y) add z)}
            ,/*4*/{(x belongsTo R) & (y belongsTo R)} --> {(x add y) <=> (y add x)}
        )

        val eng = new TransformationEngine(a add _0, statements, rules)

        var res: Option[Set[Predicate]] = eng.next2()
        while (res.isDefined) {
            res.foreach(_.foreach(pr => println("res>>> " + pr)))
            res = eng.next2()
        }
    }
}
