package org.igye.logic

import org.igye.logic.LogicalExpressions.{applySubstitution, createSubstitution}
import org.igye.logic.graph.common.{GraphTraverser, Node, NodeProcessor}
import org.igye.logic.graph.queryengine._

class QueryEngine(queryPr: Predicate, predicateStorage: PredicateStorage, ruleStorage: RuleStorage) extends NodeProcessor {
    private var nodeCnt: Int = 0

    val traverser = new GraphTraverser(List(RootNode(queryPr, nextNodeCnt())), this)

    private def nextNodeCnt() = {
        nodeCnt += 1
        nodeCnt
    }

    def execute(): List[Result] = {
        while (traverser.step()){}
        traverser.getResults().map(_.asInstanceOf[Result])
    }

    def getProcessedNodes = traverser.getProcessedNodes

    override def isResult(node: Node): Boolean = node.isInstanceOf[Result]

    override def process(node: Node): List[Node] = node match {
        case r: Result => Nil
        case rn: RootNode =>
            predicateStorage.getTrueStatements.flatMap(createSubstitution(rn.query, _)).map(Result(rn, _, nextNodeCnt())):::
            ruleStorage.getSubRules.flatMap{sr=>
                createSubstitution(rn.query, sr.result).map(RuleHead(rn, sr, _, nextNodeCnt()))
            }
        case rh: RuleHead =>
            predicateStorage.getTrueStatements.flatMap(createSubstitution(rh.query.head, _)).map{sbt=>
                if (rh.query.tail.isEmpty) {
                    createNodeFromTerminalRuleNode(rh, sbt)
                } else {
                    RuleTail(rh, sbt, rh.query.tail.map(applySubstitution(_, sbt)), nextNodeCnt())
                }
            }:::
            ruleStorage.getSubRules.flatMap{sr=>
                createSubstitution(rh.query.head, sr.result).map(RuleHead(rh, sr, _, nextNodeCnt()))
            }
        case rt: RuleTail =>
            predicateStorage.getTrueStatements.flatMap(createSubstitution(rt.query.head, _)).map{sbt=>
                if (rt.query.tail.isEmpty) {
                    createNodeFromTerminalRuleNode(rt, sbt.concat(rt.collectedSubsts))
                } else {
                    val newCollectedSubst = sbt.concat(rt.collectedSubsts)
                    RuleTail(rt, newCollectedSubst, rt.query.tail.map(applySubstitution(_, newCollectedSubst)), nextNodeCnt())
                }
            }:::
            ruleStorage.getSubRules.flatMap{sr=>
                createSubstitution(rt.query.head, sr.result).map(RuleHead(rt, sr, _, nextNodeCnt()))
            }
    }

    private def findParentNodeToContinueWorkWith(node: RuleNode, collectedSubsts: Substitution): (Node, Substitution) = node match {
        case rt: RuleTail => findParentNodeToContinueWorkWith(rt.parent, collectedSubsts)
        case rh: RuleHead =>
            val newCollectedSubstitutions = rh.gate.replaceValues(collectedSubsts)
            rh.parent match {
                case rt: RuleTail =>
                    if (rt.query.tail.nonEmpty) {
                        (rt, newCollectedSubstitutions)
                    } else {
                        findParentNodeToContinueWorkWith(rt, newCollectedSubstitutions.concat(rt.collectedSubsts))
                    }
                case rh: RuleHead =>
                    if (rh.rule.conjSet.tail.nonEmpty) {
                        (rh, newCollectedSubstitutions)
                    } else {
                        findParentNodeToContinueWorkWith(rh, newCollectedSubstitutions)
                    }
                case rn: RootNode => (rn, newCollectedSubstitutions)
            }
    }

    private def createNodeFromTerminalRuleNode(terminalRuleNode: RuleNode, overallSubst: Substitution): Node with Product with Serializable = {
        findParentNodeToContinueWorkWith(terminalRuleNode, overallSubst) match {
            case (rn: RootNode, s: Substitution) => Result(terminalRuleNode, s, nextNodeCnt())
            case (rh: RuleHead, s: Substitution) =>
                RuleTail(rh, s, rh.query.tail.map(applySubstitution(_, s)), nextNodeCnt())
            case (rt: RuleTail, s: Substitution) =>
                val newCollectedSubst = rt.collectedSubsts.concat(s)
                RuleTail(
                    rt,
                    newCollectedSubst,
                    rt.query.tail.map(applySubstitution(_, newCollectedSubst)),
                    nextNodeCnt()
                )
        }
    }
}
