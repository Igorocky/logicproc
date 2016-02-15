package org.igye.logic

import org.igye.logic.LogicalOperationsOnPredicate.predicateToLogicalOperationsOnPredicate
import org.junit.Test

class TransformationEngineTest {
    @Test
    def next(): Unit = {
        val R = StringPredicate("R")
        val _0 = StringPredicate("0")
        val a = StringPredicate("a")

        val x = Placeholder("x")
        val y = Placeholder("y")

        val statements = new PredicateStorage(
            _0 belongsTo R
            ,a belongsTo R
        )

        val rules = new RuleStorage(
            {x belongsTo R} --> {(x add _0) <=> x}
            ,{x belongsTo R} --> {(x add _0) <=> (_0 add x)}
        )

        val eng = new TransformationEngine(a add _0, statements, rules)

        var res = eng.next2()
        while (res.isDefined) {
            println("----------------------------")
            res.foreach(_.foreach(println))
            res = eng.next2()
        }
    }
}
