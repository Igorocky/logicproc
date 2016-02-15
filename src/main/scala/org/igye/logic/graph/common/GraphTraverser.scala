package org.igye.logic.graph.common

class GraphTraverser(initialNodes: List[Node], proc: NodeProcessor) {
    private var nonProcessedNodes = initialNodes.sortWith(_ < _)
    private var processedNodes = List[Node]()
    private var results = List[Node]()

    def getResults() = {
        val res = results
        results = Nil
        res
    }

    def step(): Boolean = {
        if (nonProcessedNodes.nonEmpty) {
            val curNode = nonProcessedNodes.head
            nonProcessedNodes = nonProcessedNodes.tail
            processedNodes ::= curNode
            val newNodes = proc.process(curNode).filter(newNode=>
                !processedNodes.contains(newNode) && !nonProcessedNodes.contains(newNode)
            )
            newNodes.foreach{newNode=>
                if (proc.isResult(newNode)) {
                    results ::= newNode
                }
            }
            nonProcessedNodes :::= newNodes
            nonProcessedNodes = nonProcessedNodes.sortWith(_ < _)
            true
        } else {
            false
        }
    }

    def getProcessedNodes = processedNodes
}
