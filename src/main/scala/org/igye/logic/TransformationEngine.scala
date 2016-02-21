package org.igye.logic

import org.igye.logic.LogicalExpressions.{applySubstitution, findSubStructures}
import org.igye.logic.graph.common.{GraphTraverser, Node, NodeProcessor}
import org.igye.logic.graph.transfengine.{PossibleTransformation, TransfResult}
import org.igye.logic.predicates.common.{eqTo, eqToBid}

class TransformationEngine(startPr: Predicate, predicateStorage: PredicateStorage, ruleStorage: RuleStorage) extends NodeProcessor {
    private val traverser = new GraphTraverser(Set(TransfResult(startPr)), this)

    def next2(): Option[Set[Predicate]] = {
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
                    part = foundSubStructure,
                    rule = rule,
                    eqLeft = eqLeft,
                    subs = subs
                )
        }
        log("results in " + res)
        res
    }

    private def replace(where: Predicate, what: Predicate, withWhat: Predicate): Predicate = {
        if (where eq what) {
            withWhat
        } else {
            where.copy(where.orderedChildren.map(replace(_, what, withWhat)))
        }
    }

    private def isSeqTrue(set: Set[Predicate], subs: Option[Substitution] = None): Set[Substitution] = {
        val newPr = if (subs.isDefined) applySubstitution(set.head, subs.get) else set.head
        val newSubs = isTrue(newPr).map(s => if (subs.isDefined) s.concat(subs.get) else s)
        if (set.tail.isEmpty) {
            newSubs
        } else {
            newSubs.flatMap(s => isSeqTrue(set.tail, Some(s)))
        }
    }

    private def isTrue(pr: Predicate): Set[Substitution] = {
        new QueryEngine(pr, predicateStorage, ruleStorage).execute().map(_.subst)
    }

    override def isResult(state: Any): Boolean = state.isInstanceOf[TransfResult]

    override def process(state: Any): Set[Any] = state match {
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
            }.toSet
        case posTr: PossibleTransformation =>
            val condition = posTr.rule.conjSet.map(applySubstitution(_, posTr.subs))
            log("querying for " + condition)
            val isTrue = isSeqTrue(condition)
            log(condition + " isTrue = " + isTrue)
            val eqRight = posTr.rule.result match {
                case e: eqTo => e.right
                case e: eqToBid => if (posTr.eqLeft == e.left) e.right else e.left
            }
            isTrue.map(s=>
                replace(parentOf(posTr).predicate, posTr.part, applySubstitution(eqRight, posTr.subs.concat(s)))
            ).map(TransfResult(_))
    }

    private def parentOf(posTr: PossibleTransformation) = traverser.getParent(posTr).get.asInstanceOf[TransfResult]

    private def log(msg: String): Unit = {
//        println("-----------------------------------------")
        println("log: " + msg)
    }

    override def getNextState(unprocessedStates: List[Any]): Any = unprocessedStates.last
}
