package org.igye.logic.graph

import org.igye.logic.Substitution

case class Result(parent: Node, subst: Substitution, orderNumber: Int) extends Node