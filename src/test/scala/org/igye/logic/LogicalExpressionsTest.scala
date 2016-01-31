package org.igye.logic

import org.igye.logic.LogicalOperationsOnPredicate.predicateToLogicalOperationsOnPredicate
import org.junit.{Assert, Test}

class LogicalExpressionsTest {
    val A = StringPredicate("A")
    val B = StringPredicate("B")
    val C = StringPredicate("C")
    val D = StringPredicate("D")
    val E = StringPredicate("E")

    val X = Placeholder("X")
    val Y = Placeholder("Y")
    val Z = Placeholder("Z")

    @Test
    def disjToList(): Unit = {
        val disj = A&B or B&C or C&D or D
        val list = new LogicalExpressions(null).disjToList(disj)
        Assert.assertEquals(A&B, list(0))
        Assert.assertEquals(B&C, list(1))
        Assert.assertEquals(C&D, list(2))
        Assert.assertEquals(D, list(3))
    }

    @Test
    def conjToList(): Unit = {
        val conj = A & B & C & D
        val list = new LogicalExpressions(null).conjToList(conj)
        Assert.assertEquals(A, list(0))
        Assert.assertEquals(B, list(1))
        Assert.assertEquals(C, list(2))
        Assert.assertEquals(D, list(3))
    }

    @Test
    def eval(): Unit = {
        var stor = new PredicateStorage
        stor.saveTrue(A)
        stor.saveFalse(B)
        var expr = new LogicalExpressions(stor)
        Assert.assertTrue(expr.eval(A or B).get)
        Assert.assertFalse(expr.eval(A & B).get)
        Assert.assertFalse(expr.eval(A & B & C).get)
        Assert.assertEquals(None, expr.eval(A & C))

        stor = new PredicateStorage
        stor.saveFalse(A)
        stor.saveTrue(B)
        stor.saveTrue(C)
        expr = new LogicalExpressions(stor)
        Assert.assertFalse(expr.eval(!(A or B&C)).get)

        stor = new PredicateStorage
        stor.saveFalse(A)
        stor.saveFalse(B)
        stor.saveFalse(C)
        expr = new LogicalExpressions(stor)
        Assert.assertTrue(expr.eval(!(A or B&C)).get)

        stor = new PredicateStorage
        stor.saveTrue(B)
        stor.saveFalse(C)
        expr = new LogicalExpressions(stor)
        Assert.assertEquals(None, expr.eval(!(A or B&C)))
    }

    @Test
    def createSubstitutions(): Unit = {
        var subs = new LogicalExpressions(null).createSubstitutions(
            (X is B) & (Y is D),
            (A is B) & (C is D)
        )
        Assert.assertEquals(A, subs.get(X))
        Assert.assertEquals(C, subs.get(Y))

        subs = new LogicalExpressions(null).createSubstitutions(
            (X is B) & (X is D),
            (A is B) & (C is D)
        )
        Assert.assertEquals(None, subs)
    }

    @Test
    def applyRule1(): Unit = {
        val stor = new PredicateStorage
        stor.save(A is B)
        stor.save(A is C)

        val newPredicates = new LogicalExpressions(stor).applyRule(
            ((X is B) & (X is C)) ==> (X is D)
        )
        Assert.assertEquals(1, newPredicates.length)
        Assert.assertEquals(A is D, newPredicates(0))
    }

    @Test
    def applyRule2(): Unit = {
        val stor = new PredicateStorage
        stor.save(A is B)
        stor.save(B is C)

        val newPredicates = new LogicalExpressions(stor).applyRule(
            ((X is Y) & (Y is Z)) ==> (X is Z)
        )
        Assert.assertEquals(1, newPredicates.length)
        Assert.assertEquals(A is C, newPredicates(0))
    }
}
