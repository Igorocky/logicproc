package org.igye.logic

import org.igye.logic.LogicalOperationsOnPredicate.predicateToLogicalOperationsOnPredicate
import org.junit.{Assert, Test}

class PredicateStorageTest {
    val A = StringPredicate("A")
    val B = StringPredicate("B")
    val C = StringPredicate("C")
    val D = StringPredicate("D")
    val E = StringPredicate("E")

    @Test
    def getTrueOrFalse(): Unit = {
        val storage = new PredicateStorage

        storage.save(A)
        Assert.assertTrue(storage.getTrueOrFalse(A).get)

        storage.saveTrue(!(A&B))
        Assert.assertFalse(storage.getTrueOrFalse(A&B).get)

        storage.saveFalse(C)
        Assert.assertTrue(storage.getTrueOrFalse(!C).get)

        storage.save(C is D)
        Assert.assertFalse(storage.getTrueOrFalse(!(C is D)).get)

        storage.saveTrue(!(!(!(E))))
        Assert.assertFalse(storage.getTrueOrFalse(E).get)
        Assert.assertTrue(storage.getTrueOrFalse(!(!(!(E)))).get)
        Assert.assertFalse(storage.getTrueOrFalse(!(!(!(!(E))))).get)
    }

    @Test
    def saveIsStmtThrowsException(): Unit = {
        val storage = new PredicateStorage
        try {
            storage.saveTrue((A is B) & !(C is D))
            storage.saveFalse((A is B) & !(C is D))
            Assert.fail("exception should be thrown")
        } catch {
            case ex: IllegalArgumentException => Assert.assertEquals(
                "statement [(A is B) & !(C is D) is false] contradicts to [(A is B) & !(C is D) is true]",
                ex.getMessage
            )
            case ex: Throwable => Assert.fail(s"IllegalArgumentException should be thrown, but was $ex")
        }
    }

}
