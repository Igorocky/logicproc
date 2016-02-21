package org.igye.logic.graph.common

class GraphTraverser(initialStates: Set[Any], proc: NodeProcessor) {
    private var nonProcessedNodes: List[Node] = initialStates.map(Node(null, _)).toList
    private var processedNodes: List[Node] = List[Node]()
    private var results: Set[Any] = initialStates.filter(proc.isResult)

    def getResults(): Set[Any] = {
        val res = results
        results = Set()
        res
    }

    def step(): Boolean = {
        if (nonProcessedNodes.nonEmpty) {
            val currState = proc.getNextState(nonProcessedNodes.map(_.value))
            val (nonProcessedNodesVal, currNode) = nonProcessedNodes.foldLeft((List[Node](), null: Node)){
                case ((list, currNode), node) => if (node.value == currState) (list, node) else (node::list, currNode)
            }
            nonProcessedNodes = nonProcessedNodesVal.reverse
            processedNodes ::= currNode
            val newStates = proc.process(currState).filter(newState=>
                !processedNodes.exists(_.value == newState)
                    && !nonProcessedNodes.exists(_.value == newState)
                    && proc.isAllowed(newState)
            )
            newStates.foreach{newState=>
                if (proc.isResult(newState)) {
                    results += newState
                }
            }
            nonProcessedNodes :::= newStates.toList.map(Node(currNode, _))
            true
        } else {
            false
        }
    }

    def getProcessedNodes: List[Node] = processedNodes

    def getParent(state: Any): Option[Any] = {
        processedNodes.find(_.value == state).map(_.parent.value)
    }
}
