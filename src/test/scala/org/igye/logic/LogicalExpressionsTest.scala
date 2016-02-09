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
        val list = new LogicalExpressions().disjToList(disj)
        Assert.assertEquals(A&B, list(0))
        Assert.assertEquals(B&C, list(1))
        Assert.assertEquals(C&D, list(2))
        Assert.assertEquals(D, list(3))
    }

    @Test
    def conjToList(): Unit = {
        val conj = A & B & C & D
        val list = new LogicalExpressions().conjToList(conj)
        Assert.assertEquals(A, list(0))
        Assert.assertEquals(B, list(1))
        Assert.assertEquals(C, list(2))
        Assert.assertEquals(D, list(3))
    }

    @Test
    def eval1(): Unit = {
        val stor = new PredicateStorage
        stor.saveTrue(A)
        stor.saveFalse(B)
        val expr = new LogicalExpressions(stor)
        Assert.assertTrue(expr.eval(A or B).get)
        Assert.assertFalse(expr.eval(A & B).get)
        Assert.assertFalse(expr.eval(A & B & C).get)
        Assert.assertEquals(None, expr.eval(A & C))
    }

    @Test
    def eval2(): Unit = {
        val stor = new PredicateStorage
        stor.saveFalse(A)
        stor.saveTrue(B)
        stor.saveTrue(C)
        val expr = new LogicalExpressions(stor)
        Assert.assertFalse(expr.eval(!(A or B&C)).get)
    }

    @Test
    def eval3(): Unit = {
        val stor = new PredicateStorage
        stor.saveFalse(A)
        stor.saveFalse(B)
        stor.saveFalse(C)
        val expr = new LogicalExpressions(stor)
        Assert.assertTrue(expr.eval(!(A or B&C)).get)
    }

    @Test
    def eval4(): Unit = {
        val stor = new PredicateStorage
        stor.saveTrue(B)
        stor.saveFalse(C)
        val expr = new LogicalExpressions(stor)
        Assert.assertEquals(None, expr.eval(!(A or B&C)))
    }

    @Test
    def eval5(): Unit = {
        val stor = new PredicateStorage
        stor.saveFalse(B)
        val expr = new LogicalExpressions(stor)
        Assert.assertFalse(expr.eval(A&B).get)
    }

    @Test
    def createSubstitutions1(): Unit = {
        val sub1 = new LogicalExpressions().createSubstitution(
            (X is B) & (Y is D),
            (A is B) & (C is D)
        ).get
        Assert.assertEquals(A, sub1.get(X).get)
        Assert.assertEquals(C, sub1.get(Y).get)
    }

    @Test
    def createSubstitutions2(): Unit = {
        val sub2 = new LogicalExpressions().createSubstitution(
            (X is B) & (X is D),
            (A is B) & (C is D)
        )
        Assert.assertEquals(None, sub2)
    }

    @Test
    def createSubstitutions3(): Unit = {
        val sub1 = new LogicalExpressions().createSubstitution(
            (X is B) & (Y is D),
            (A is B) & (Z is D)
        ).get
        Assert.assertEquals(A, sub1.get(X).get)
        Assert.assertEquals(Z, sub1.get(Y).get)
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

    @Test
    def query1(): Unit = {
        val qRes = new LogicalExpressions(
            new PredicateStorage(
                (A is E) & (A is D)
                ,(C is E) & (C is D)
                ,(C is E) & (B is D)
            )
        ).query(
            (X is E) & (X is D)
        )
        Assert.assertEquals(2, qRes.length)
        Assert.assertTrue(qRes.contains(Map(X -> A)))
        Assert.assertTrue(qRes.contains(Map(X -> C)))
    }
}
