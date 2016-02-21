package org.igye.logic

import org.igye.logic.graph.common.Node
import org.igye.logic.graph.queryengine.{Result, RootNode, RuleHead, RuleTail}

object ResultUtils {
    /*def explain(processedNodes: List[Node], nodesToMark: List[Node] = Nil): String = {
        val root = getPathToRoot(processedNodes.head).last
        printNode(root, nodesToMark) + "\n" + printChildNodes(root, 1, processedNodes, nodesToMark)
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

    private def printChildNodes(node: Node, level: Int, allNodes: List[Node], nodesToMark: List[Node]): String = {
        getAllChildren(node, allNodes).sortWith(_ < _).map{c=>
            "  "*level + printNode(c, nodesToMark) + "\n" + printChildNodes(c, level + 1, allNodes, nodesToMark)
        }.mkString("")
    }

    private def printNode(node: Node, nodesToMark: List[Node]): String = {
        val mark = if (nodesToMark.exists(_ eq node)) {
            "*" + nodesToMark.indexWhere(_ eq node) + "*"
        } else {
            ""
        }
        node match {
            case rn: RootNode => s"[${rn.orderNumber}]${mark}Root: ${rn.query}"
            case rh: RuleHead => s"[${rh.orderNumber}]${mark}RH: q: ${rh.query}, r: ${rh.rule}, a: ${rh.collectedSubsts.flattenMap}, g: ${rh.gate.flattenMap}"
            case rt: RuleTail => s"[${rt.orderNumber}]${mark}RT: q: ${rt.query}, a: ${rt.collectedSubsts.flattenMap}"
            case r: Result => s"[${r.orderNumber}]${mark}Result: ${r.subst.flattenMap}"
        }
    }*/
}
