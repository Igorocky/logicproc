package org.igye.logic.graph.transfengine

import org.igye.logic.Predicate
import org.igye.logic.graph.common.Node

case class TransfResult(parent: PossibleTransformation, predicate: Predicate, orderNumber: Int) extends Node {
    override def equals(other: Any): Boolean = {
        if (other == null) {
            false
        } else {
            other match {
                case TransfResult(parent, pr, _) =>
                    this.predicate == pr
                case _ => false
            }
        }
    }
}