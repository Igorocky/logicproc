package org.igye.logic.graph.common

trait NodeProcessor {
    def isResult(state: Any): Boolean
    def isAllowed(state: Any): Boolean
    def process(state: Any): Set[Any]
    def getNextState(unprocessedStates: List[Any]): Any
}
