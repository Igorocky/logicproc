package org.igye.logic

import org.igye.logic.graph.common.{Node, NodeProcessor}

class TransformationEngine(startPr: Predicate, predicateStorage: PredicateStorage, ruleStorage: RuleStorage) extends NodeProcessor {
    override def isResult(node: Node): Boolean = ???

    override def process(node: Node): List[Node] = ???
}
