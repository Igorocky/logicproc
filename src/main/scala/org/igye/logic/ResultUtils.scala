package org.igye.logic

import org.igye.logic.graph.common.{GraphTraverser, Node}
import org.igye.logic.graph.queryengine.{Result, RootNode, RuleHead, RuleTail}

object ResultUtils {
    def explain(graphTraverser: GraphTraverser, statesToMark: List[Any] = Nil): String = {
        val root = getPathToRoot(graphTraverser.getProcessedNodes.head.value, graphTraverser).last
        printNode(root, statesToMark) + "\n" + printChildStates(root, 1, graphTraverser, statesToMark)
    }

    private def findSuccessfulNodes(res: Result, graphTraverser: GraphTraverser): List[Any] = {
        getPathToRoot(res, graphTraverser)
    }

    private def getPathToRoot(startNode: Any, graphTraverser: GraphTraverser): List[Any] = startNode match {
        case rn: RootNode => List(rn)
        case a: Any => a::getPathToRoot(graphTraverser.getParent(a).get, graphTraverser)
    }

    private def getAllChildren(parentState: Any, graphTraverser: GraphTraverser): List[Any] = {
        graphTraverser.getProcessedNodes.filter(n => n.parent != null && n.parent.value == parentState).map(_.value)
    }

    private def printChildStates(state: Any, level: Int, graphTraverser: GraphTraverser, statesToMark: List[Any]): String = {
        getAllChildren(state, graphTraverser).map{c=>
            "  "*level + printNode(c, statesToMark) + "\n" + printChildStates(c, level + 1, graphTraverser, statesToMark)
        }.mkString("")
    }

    private def printNode(node: Any, nodesToMark: List[Any]): String = {
        val mark = if (nodesToMark.exists(_ == node)) {
            "*" + nodesToMark.indexWhere(_ == node) + "*"
        } else {
            ""
        }
        node match {
            case rn: RootNode => s"${mark}Root: ${rn.query}"
            case rh: RuleHead => s"${mark}RH: q: ${rh.query}, r: ${rh.rule}, a: ${rh.collectedSubsts.flattenMap}, g: ${rh.gate.flattenMap}"
            case rt: RuleTail => s"${mark}RT: q: ${rt.query}, a: ${rt.collectedSubsts.flattenMap}"
            case r: Result => s"${mark}Result: ${r.subst.flattenMap}"
        }
    }
}
