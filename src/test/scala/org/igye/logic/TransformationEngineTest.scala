package org.igye.logic

import jdk.nashorn.internal.ir.annotations.Ignore
import org.igye.logic.LogicalOperationsOnPredicate.predicateToLogicalOperationsOnPredicate
import org.igye.logic.graph.transfengine.TransfResult
import org.igye.logic.predicates.PredicateUtils
import org.igye.logic.predicates.math.{addInv, mulInv}
import org.junit.{Assert, Test}

class TransformationEngineTest {
  @Ignore
  @Test
  def next(): Unit = {
    val R = StringPredicate("R")
    val Rsub0 = StringPredicate("Rsub0")
    val _0 = StringPredicate("0")
    val _1 = StringPredicate("1")
    val a = StringPredicate("a")

    val x = Placeholder("x")
    val y = Placeholder("y")
    val z = Placeholder("z")

    val statements = new PredicateStorage(
      _0 belongsTo R
      , _1 belongsTo Rsub0
      , a belongsTo R
    )

    val rules = new RuleStorage(
      {(x belongsTo R) & (y belongsTo R)} --> {(x add y) belongsTo R}
      , {x belongsTo R} --> {addInv(x) belongsTo R}
      , {(x belongsTo R) & (y belongsTo R)} --> {(x mul y) belongsTo R}
      , {x belongsTo R} --> {mulInv(x) belongsTo R}
      , {x belongsTo Rsub0} --> {x belongsTo R}

      , /*1*/ {x belongsTo R} --> {(x add _0) <=> x}
      , /*2*/ {x belongsTo R} --> {(x add addInv(x)) <=> _0}
      , /*3*/ {(x belongsTo R) & (y belongsTo R) & (z belongsTo R)} --> {(x add (y add z)) <=> ((x add y) add z)}
      , /*4*/ {(x belongsTo R) & (y belongsTo R)} --> {(x add y) <=> (y add x)}

      , /*1*/ {x belongsTo R} --> {(x mul _1) <=> x}
      , /*2*/ {x belongsTo Rsub0} --> {(x mul mulInv(x)) <=> _1}
      , /*3*/ {(x belongsTo R) & (y belongsTo R) & (z belongsTo R)} --> {(x mul (y mul z)) <=> ((x mul y) mul z)}
      , /*4*/ {(x belongsTo R) & (y belongsTo R)} --> {(x mul y) <=> (y mul x)}

      , /*5*/ {(x belongsTo R) & (y belongsTo R) & (z belongsTo R)} --> {((x add y) mul z) <=> ((x mul z) add (y mul z))}
    )

    val restrictions = List[Any => Boolean](
      _ match {
        case tr: TransfResult => PredicateUtils.calcDepth(tr.predicate) > 5
        case _ => false
      }
    )

    val eng = new TransformationEngine(a mul _0, statements, rules, restrictions)

    var overallRes = Set[Predicate]()
    var res: Option[Set[Predicate]] = eng.next2()
    var cnt = 0
    while (res.isDefined) {
      overallRes ++= res.get
      Assert.assertFalse(res.get.contains(_0))
      res.foreach(_.foreach{pr =>
        cnt += 1
        println(s"res[$cnt]>>> " + pr + s" by ${eng.getRule(pr)}")
      })
      res = eng.next2()
    }
    Assert.assertEquals(49, overallRes.size)
  }
}
