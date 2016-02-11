package org.igye.logic

import org.igye.logic.LogicalOperationsOnPredicate.predicateToLogicalOperationsOnPredicate
import org.junit.{Assert, Test}

class QueryEngineTest {
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
    def query1(): Unit = {
        implicit val stor = new PredicateStorage(
            (A is E) & (A is D)
            ,(C is E) & (C is D)
            ,(C is E) & (B is D)
        )
        val qRes = QueryEngine.query(
            Set((X is E) & (X is D))
        ).map(_.flattenMap)
        Assert.assertEquals(2, qRes.length)
        Assert.assertTrue(qRes.contains(Map(X -> A)))
        Assert.assertTrue(qRes.contains(Map(X -> C)))
    }

    @Test
    def createEquivalentQueries(): Unit = {
        implicit val rules = new RuleStorage(
            ((A or B) & C) ==> (A & B)
        )
        val eqQueries = QueryEngine.createEquivalentQueries(M & N)
        Assert.assertEquals(2, eqQueries.size)
        Assert.assertTrue(eqQueries.contains(Set(M, C)))
        Assert.assertTrue(eqQueries.contains(Set(N, C)))
    }
}
