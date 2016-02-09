package org.igye.logic.graph

trait NodeProcessor {
    def isResult(node: Node): Boolean
    def process(node: Node): List[Node]
}
