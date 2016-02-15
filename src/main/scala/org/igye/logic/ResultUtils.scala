package org.igye.logic

import org.igye.logic.graph._
import org.igye.logic.graph.common.Node
import org.igye.logic.graph.queryengine.{RuleTail, Result, RootNode, RuleHead}

object ResultUtils {
    def explain(res: Result, processedNodes: List[Node]): String = {
        val root = getPathToRoot(res).last
        printNode(root) + "\n" + printChildNodes(root, 1, processedNodes)
    }

    private def findSuccessfulNodes(res: Result, processedNodes: List[Node]): List[Node] = {
        getPathToRoot(res)
    }

    private def getPathToRoot(startNode: Node): List[Node] = startNode match {
        case rn: RootNode => List(rn)
        case n: Node => n::getPathToRoot(n.parent)
    }

    private def getAllChildren(parentNode: Node, allNodes: List[Node]): List[Node] = {
        allNodes.filter(_.parent == parentNode)
    }

    private def printChildNodes(node: Node, level: Int, allNodes: List[Node]): String = {
        getAllChildren(node, allNodes).map{c=>
            "  "*level + printNode(c) + "\n" + printChildNodes(c, level + 1, allNodes)
        }.mkString("\n")
    }

    private def printNode(node: Node): String = node match {
        case rn: RootNode => s"Root: ${rn.query}"
        case rh: RuleHead => s"RH: r: ${rh.rule}, g: ${rh.gate.flattenMap}"
        case rt: RuleTail => s"RT: q: ${rt.query}, a: ${rt.collectedSubsts.flattenMap}"
        case r: Result => s"Result: ${r.subst.flattenMap}"
    }
}
