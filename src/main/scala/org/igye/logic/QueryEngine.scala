package org.igye.logic

import org.igye.logic.LogicalExpressions.{applySubstitution, createSubstitution}
import org.igye.logic.graph.common.{GraphTraverser, Node, NodeProcessor}
import org.igye.logic.graph.queryengine._

class QueryEngine(queryPr: Predicate, predicateStorage: PredicateStorage, ruleStorage: RuleStorage) extends NodeProcessor {
    val traverser = new GraphTraverser(Set(RootNode(queryPr)), this)

    def execute(): Set[Result] = {
        while (traverser.step()){}
        traverser.getResults().map(_.asInstanceOf[Result])
    }

    def getProcessedNodes = traverser.getProcessedNodes

    override def isResult(state: Any): Boolean = state.isInstanceOf[Result]

    override def process(state: Any): Set[Any] = state match {
        case r: Result => Set()
        case rn: RootNode =>
            (predicateStorage.getTrueStatements.flatMap(createSubstitution(rn.query, _)).map(Result(_)) :::
            ruleStorage.getSubRules.flatMap{sr=>
                createSubstitution(rn.query, sr.result).filter(gateFilter).map{subs=>
                    RuleHead(invertAndRemovePlaceholders(subs), sr, createGate(subs))
                }
            }).toSet
        case rh: RuleHead =>
            (predicateStorage.getTrueStatements.flatMap(createSubstitution(rh.query.head, _)).map{sbt=>
                if (rh.query.tail.isEmpty) {
                    createNodeFromTerminalRuleNode(rh, sbt)
                } else {
                    val newCollectedSubst = sbt.concat(rh.collectedSubsts)
                    RuleTail(rh.rule, newCollectedSubst, rh.query.tail.map(applySubstitution(_, newCollectedSubst)))
                }
            }:::
            ruleStorage.getSubRules.flatMap{sr=>
                createSubstitution(rh.query.head, sr.result).filter(gateFilter).map{subs=>
                    RuleHead(invertAndRemovePlaceholders(subs), sr, createGate(subs))
                }
            }).toSet
        case rt: RuleTail =>
            (predicateStorage.getTrueStatements.flatMap(createSubstitution(rt.query.head, _)).map{sbt=>
                if (rt.query.tail.isEmpty) {
                    createNodeFromTerminalRuleNode(rt, sbt.concat(rt.collectedSubsts))
                } else {
                    val newCollectedSubst = sbt.concat(rt.collectedSubsts)
                    RuleTail(rt.rule, newCollectedSubst, rt.query.tail.map(applySubstitution(_, newCollectedSubst)))
                }
            }:::
            ruleStorage.getSubRules.flatMap{sr=>
                createSubstitution(rt.query.head, sr.result).filter(gateFilter).map{subs=>
                    RuleHead(invertAndRemovePlaceholders(subs), sr, createGate(subs))
                }
            }).toSet
    }

    private def gateFilter(subs: Substitution): Boolean = {
        subs.flattenMap.isEmpty ||
            subs.flattenMap.forall{case (k,v) => v.isInstanceOf[StringPredicate] || v.isInstanceOf[Placeholder]}
    }

    private def invertAndRemovePlaceholders(subs: Substitution) = {
        Substitution(
            subs.flattenMap.map{case (k,v) => (v,k)}.filter{
                case (k,v: Placeholder) => false
                case _ => true
            },
            None
        )
    }

    private def createGate(subs: Substitution) = {
        Substitution(
            subs.flattenMap.filter{
                case (k,v) => k.isInstanceOf[Placeholder]
            },
            None
        )
    }

    private def parentOf(rt: RuleTail) = traverser.getParent(rt).get.asInstanceOf[RuleNode]
    private def parentOf(rh: RuleHead) = traverser.getParent(rh).get

    private def findParentStateToContinueWorkWith(terminalRuleNode: RuleNode,
                                                  rule: SubRule, collectedSubsts: Substitution): (Any, Substitution) = terminalRuleNode match {
        case rt: RuleTail => findParentStateToContinueWorkWith(parentOf(rt), rule, collectedSubsts)
        case rh: RuleHead if (rule != null && rule != rh.rule) =>
            findParentStateToContinueWorkWith(parentOf(rh).asInstanceOf[RuleNode], rule, collectedSubsts)
        case rh: RuleHead =>
            val newCollectedSubstitutions = rh.gate.replaceValues(collectedSubsts)
            parentOf(rh) match {
                case rt: RuleTail =>
                    if (rt.query.tail.nonEmpty) {
                        (rt, newCollectedSubstitutions)
                    } else {
                        findParentStateToContinueWorkWith(rt, rt.rule, newCollectedSubstitutions.concat(rt.collectedSubsts))
                    }
                case rh: RuleHead =>
                    if (rh.rule.conjSet.tail.nonEmpty) {
                        (rh, newCollectedSubstitutions)
                    } else {
                        findParentStateToContinueWorkWith(rh, null, newCollectedSubstitutions)
                    }
                case rn: RootNode => (rn, newCollectedSubstitutions)
            }
    }

    private def getRule(ruleNode: RuleNode) = ruleNode match {
        case rh: RuleHead => null
        case rt: RuleTail => rt.rule
    }

    private def createNodeFromTerminalRuleNode(terminalRuleNode: RuleNode, overallSubst: Substitution) = {
//        println("----------------------------------------------------------------------")
//        println(ResultUtils.explain(getProcessedNodes, List(terminalRuleNode)))
        findParentStateToContinueWorkWith(terminalRuleNode, getRule(terminalRuleNode), overallSubst) match {
            case (rn: RootNode, s: Substitution) => Result(s)
            case (rh: RuleHead, s: Substitution) =>
                RuleTail(rh.rule, s, rh.query.tail.map(applySubstitution(_, s)))
            case (rt: RuleTail, s: Substitution) =>
                val newCollectedSubst = rt.collectedSubsts.concat(s)
                RuleTail(
                    rt.rule,
                    newCollectedSubst,
                    rt.query.tail.map(applySubstitution(_, newCollectedSubst))
                )
        }
    }

    override def getNextState(unprocessedStates: List[Any]): Any = unprocessedStates.last
}
