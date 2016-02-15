package org.igye.logic.graph.queryengine

import org.igye.logic.Substitution
import org.igye.logic.graph.common.Node

case class Result(parent: Node, subst: Substitution, orderNumber: Int) extends Node