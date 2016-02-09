package org.igye.logic.graph

class GraphTraverser(initialNodes: List[Node], proc: NodeProcessor) {
    private var nonProcessedNodes = initialNodes.sorted
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
                    results ::= curNode
                }
            }
            nonProcessedNodes :::= newNodes
            nonProcessedNodes = nonProcessedNodes.sorted
            true
        } else {
            false
        }
    }
}
