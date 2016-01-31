package org.igye.logic

import org.igye.logic.LogicOperators._
import org.igye.logic.LogicalOperationsOnPredicate.predicateToLogicalOperationsOnPredicate
import org.junit.{Assert, Test}

class LogicOperatorsTest {
    val A = StringPredicate("A")
    val B = StringPredicate("B")
    val C = StringPredicate("C")
    val D = StringPredicate("D")
    val E = StringPredicate("E")

    @Test
    def toDnfTest(): Unit = {
        checkDnf(
            (A or B or C) & D & E
            ,"(A  or  B  or  C) & D & E"
            ,{case A & D & E  or  B & D & E  or  C & D & E => true}
            ,"A & D & E  or  B & D & E  or  C & D & E"
        )

        checkDnf(
            !(!A)
            ,"!(!(A))"
            ,{case A => true}
            ,"A"
        )

        checkDnf(
            !(A or B&D)
            ,"!(A  or  B & D)"
            ,{case !(A) & !(B)  or  !(A) & !(D) => true}
            ,"!(A) & !(B)  or  !(A) & !(D)"
        )
    }

    private def checkDnf(expr: Predicate, toStringRes: String = null, structureTest: Any => Boolean = null, expectedStr: String = null): Unit = {
        var exprDnf = toDnf(expr)
        if (toStringRes == null) {
            println(s"$expr -> $exprDnf")
        } else {
            Assert.assertEquals(toStringRes, expr.toString)
            Assert.assertTrue(structureTest(exprDnf))
            Assert.assertEquals(expectedStr, exprDnf.toString)
        }
    }
}
