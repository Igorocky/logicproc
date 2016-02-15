package org.igye.logic.graph.common

trait NodeProcessor {
    def isResult(node: Node): Boolean
    def process(node: Node): List[Node]
}
