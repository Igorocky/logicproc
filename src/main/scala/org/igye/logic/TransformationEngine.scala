package org.igye.logic

import org.igye.logic.LogicalExpressions.{applySubstitution, findSubStructures}
import org.igye.logic.graph.common.{GraphTraverser, Node, NodeProcessor}
import org.igye.logic.graph.transfengine.{PossibleTransformation, TransfResult}
import org.igye.logic.predicates.common.{eqTo, eqToBid}

class TransformationEngine(startPr: Predicate, predicateStorage: PredicateStorage, ruleStorage: RuleStorage) extends NodeProcessor {
    private var nodeCnt: Int = 0
    private val traverser = new GraphTraverser(List(TransfResult(null, startPr, nextNodeCnt())), this)

    private def nextNodeCnt() = {
        nodeCnt += 1
        nodeCnt
    }

    def next2(): Option[List[Predicate]] = {
        if (traverser.step()) {
            Some(traverser.getResults().map(_.asInstanceOf[TransfResult].predicate))
        } else {
            None
        }
    }

    private def createPossibleTransformation(parentTransfRes: TransfResult,
                                             rule: SubRule, eqLeft: Predicate): List[PossibleTransformation] = {
        log("createPossibleTransformation for " + parentTransfRes.predicate + " using " + rule + " and eqLeft " + eqLeft)
        val res = findSubStructures(parentTransfRes.predicate, eqLeft).map {
            case (foundSubStructure, subs) =>
                PossibleTransformation(
                    parent = parentTransfRes,
                    part = foundSubStructure,
                    rule = rule,
                    eqLeft = eqLeft,
                    subs = subs,
                    orderNumber = nextNodeCnt()
                )
        }
        log("results in " + res)
        res
    }

    private def applyEquivalentTransformation(pr: Predicate, condition: Set[Predicate], eqLeft: Predicate, eqRight: Predicate): List[Predicate] = {
        findSubStructures(pr, eqLeft).flatMap {
            case (foundSubStructure, subs) =>
                if (isSeqTrue(condition.map(applySubstitution(_, subs)))) {
                    Some(replace(pr, foundSubStructure, applySubstitution(eqRight, subs)))
                } else {
                    None
                }
        }
    }

    private def replace(where: Predicate, what: Predicate, withWhat: Predicate): Predicate = {
        if (where eq what) {
            withWhat
        } else {
            where.copy(where.orderedChildren.map(replace(_, what, withWhat)))
        }
    }

    private def isSeqTrue(set: Set[Predicate]): Boolean = {
        set.forall(isTrue(_))
    }

    private def isTrue(pr: Predicate): Boolean = {
        new QueryEngine(pr, predicateStorage, ruleStorage).execute().nonEmpty
    }

    override def isResult(node: Node): Boolean = node.isInstanceOf[TransfResult]

    override def process(node: Node): List[Node] = node match {
        case trRes: TransfResult =>
            ruleStorage.getSubRules.filter{
                case SubRule(_, r: eqTo, _) => true
                case SubRule(_, r: eqToBid, _) => true
                case _ => false
            }.flatMap{
                case sr @ SubRule(condition, result: eqTo, _) =>
                    createPossibleTransformation(trRes, sr, result.left)
                case sr @ SubRule(condition, result: eqToBid, _) =>
                    createPossibleTransformation(trRes, sr, result.left):::
                    createPossibleTransformation(trRes, sr, result.right)
            }
        case posTr: PossibleTransformation =>
            val condition = posTr.rule.conjSet.map(applySubstitution(_, posTr.subs))
            log("querying for " + condition)
            val isTrue = isSeqTrue(condition)
            log(condition + " is " + isTrue)
            if (isTrue) {
                val eqRight = posTr.rule.result match {
                    case e: eqTo => e.right
                    case e: eqToBid => if (posTr.eqLeft == e.left) e.right else e.left
                }
                List(replace(posTr.parent.predicate, posTr.part, applySubstitution(eqRight, posTr.subs)))
                    .map(TransfResult(posTr, _, nextNodeCnt()))
            } else {
                Nil
            }
    }

    private def log(msg: String): Unit = {
//        println("-----------------------------------------")
        println("log: " + msg)
    }
}
