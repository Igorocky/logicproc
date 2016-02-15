package org.igye.logic

import org.igye.logic.LogicalExpressions.findSubStructures
import org.igye.logic.LogicalOperationsOnPredicate.predicateToLogicalOperationsOnPredicate
import org.igye.logic.predicates.is
import org.junit.{Assert, Test}

class LogicalExpressionsTest {
    val A = StringPredicate("A")
    val B = StringPredicate("B")
    val C = StringPredicate("C")
    val D = StringPredicate("D")
    val E = StringPredicate("E")

    val M = Placeholder("M")
    val N = Placeholder("N")
    val X = Placeholder("X")
    val Y = Placeholder("Y")
    val Z = Placeholder("Z")

    @Test
    def disjToList(): Unit = {
        val disj = A&B or B&C or C&D or D
        val list = LogicalExpressions.disjToList(disj)
        Assert.assertEquals(A&B, list(0))
        Assert.assertEquals(B&C, list(1))
        Assert.assertEquals(C&D, list(2))
        Assert.assertEquals(D, list(3))
    }

    @Test
    def conjToList(): Unit = {
        val conj = A & B & C & D
        val list = LogicalExpressions.conjToList(conj)
        Assert.assertEquals(A, list(0))
        Assert.assertEquals(B, list(1))
        Assert.assertEquals(C, list(2))
        Assert.assertEquals(D, list(3))
    }

    @Test
    def eval1(): Unit = {
        implicit val stor = new PredicateStorage
        stor.saveTrue(A)
        stor.saveFalse(B)
        Assert.assertTrue(LogicalExpressions.eval(A or B).get)
        Assert.assertFalse(LogicalExpressions.eval(A & B).get)
        Assert.assertFalse(LogicalExpressions.eval(A & B & C).get)
        Assert.assertEquals(None, LogicalExpressions.eval(A & C))
    }

    @Test
    def eval2(): Unit = {
        implicit val stor = new PredicateStorage
        stor.saveFalse(A)
        stor.saveTrue(B)
        stor.saveTrue(C)
        Assert.assertFalse(LogicalExpressions.eval(!(A or B&C)).get)
    }

    @Test
    def eval3(): Unit = {
        implicit val stor = new PredicateStorage
        stor.saveFalse(A)
        stor.saveFalse(B)
        stor.saveFalse(C)
        Assert.assertTrue(LogicalExpressions.eval(!(A or B&C)).get)
    }

    @Test
    def eval4(): Unit = {
        implicit val stor = new PredicateStorage
        stor.saveTrue(B)
        stor.saveFalse(C)
        Assert.assertEquals(None, LogicalExpressions.eval(!(A or B&C)))
    }

    @Test
    def eval5(): Unit = {
        implicit val stor = new PredicateStorage
        stor.saveFalse(B)
        Assert.assertFalse(LogicalExpressions.eval(A&B).get)
    }

    @Test
    def createSubstitutions1(): Unit = {
        val sub1 = LogicalExpressions.createSubstitution(
            (X is B) & (Y is D),
            (A is B) & (C is D)
        ).get
        Assert.assertEquals(A, sub1.get(X).get)
        Assert.assertEquals(C, sub1.get(Y).get)
    }

    @Test
    def createSubstitutions2(): Unit = {
        val sub2 = LogicalExpressions.createSubstitution(
            (X is B) & (X is D),
            (A is B) & (C is D)
        )
        Assert.assertEquals(None, sub2)
    }

    @Test
    def createSubstitutions3(): Unit = {
        val sub1 = LogicalExpressions.createSubstitution(
            (X is B) & (Y is D),
            (A is B) & (Z is D)
        ).get
        Assert.assertEquals(A, sub1.get(X).get)
        Assert.assertEquals(Z, sub1.get(Y).get)
    }

    @Test
    def createSubstitutions4(): Unit = {
        val sub1 = LogicalExpressions.createSubstitution(
            (M is N),
            (X is Z)
        ).get
        Assert.assertEquals(X, sub1.get(M).get)
        Assert.assertEquals(Z, sub1.get(N).get)
    }

    @Test
    def applyRule1(): Unit = {
        implicit val stor = new PredicateStorage
        stor.save(A is B)
        stor.save(A is C)

        val newPredicates = LogicalExpressions.applyRule(
            ((X is B) & (X is C)) --> (X is D)
        )
        Assert.assertEquals(1, newPredicates.length)
        Assert.assertEquals(A is D, newPredicates(0))
    }

    @Test
    def applyRule2(): Unit = {
        implicit val stor = new PredicateStorage
        stor.save(A is B)
        stor.save(B is C)

        val newPredicates = LogicalExpressions.applyRule(
            ((X is Y) & (Y is Z)) --> (X is Z)
        )
        Assert.assertEquals(1, newPredicates.length)
        Assert.assertEquals(A is C, newPredicates(0))
    }

    @Test
    def findSubStructuresTest1(): Unit = {
        findSubStructures(
            (A is B) belongsTo (A is C),
            X is Y
        ).map{case (p,s) => (p,s.flattenMap)} match {
            case List(
                (is(A, B), map1)
                ,(is(A, C), map2)
            ) =>
                Assert.assertEquals(2, map1.size)
                Assert.assertEquals(A, map1(X))
                Assert.assertEquals(B, map1(Y))
                Assert.assertEquals(2, map2.size)
                Assert.assertEquals(A, map2(X))
                Assert.assertEquals(C, map2(Y))
        }
    }

    @Test
    def findSubStructuresTest2(): Unit = {
        val WHERE_TO_SEARCH = (A is B) belongsTo (A is C)
        findSubStructures(
            WHERE_TO_SEARCH,
            (X is Y) belongsTo (X is Z)
        ).map{case (p,s) => (p,s.flattenMap)} match {
            case List(
                (WHERE_TO_SEARCH, map)
            ) =>
                Assert.assertEquals(3, map.size)
                Assert.assertEquals(A, map(X))
                Assert.assertEquals(B, map(Y))
                Assert.assertEquals(C, map(Z))
        }
    }

    @Test
    def findSubStructuresTest3(): Unit = {
        val WHERE_TO_SEARCH = (A is B) belongsTo (A is C)
        findSubStructures(
            WHERE_TO_SEARCH,
            (X is Y) belongsTo (M is Z)
        ).map{case (p,s) => (p,s.flattenMap)} match {
            case List(
                (WHERE_TO_SEARCH, map)
            ) =>
                Assert.assertEquals(4, map.size)
                Assert.assertEquals(A, map(X))
                Assert.assertEquals(B, map(Y))
                Assert.assertEquals(C, map(Z))
                Assert.assertEquals(A, map(M))
        }
    }

    @Test
    def findSubStructuresTest4(): Unit = {
        (findSubStructures(
            (A is B) belongsTo (E is C),
            (X is Y) belongsTo (X is Z)
        ): @unchecked) match {
            case Nil =>
        }
    }

    @Test
    def findSubStructuresTestRecursion(): Unit = {
        val WHERE_TO_SEARCH = (A is B) is (A is C)
        findSubStructures(
            WHERE_TO_SEARCH,
            (X is Y)
        ).map{case (p,s) => (p,s.flattenMap)} match {
            case List(
                (WHERE_TO_SEARCH, map1)
                ,(is(A, B), map2)
                ,(is(A, C), map3)
            ) =>
                Assert.assertEquals(2, map1.size)
                Assert.assertEquals(A is B, map1(X))
                Assert.assertEquals(A is C, map1(Y))
                Assert.assertEquals(2, map2.size)
                Assert.assertEquals(A, map2(X))
                Assert.assertEquals(B, map2(Y))
                Assert.assertEquals(2, map3.size)
                Assert.assertEquals(A, map3(X))
                Assert.assertEquals(C, map3(Y))

        }
    }
}
