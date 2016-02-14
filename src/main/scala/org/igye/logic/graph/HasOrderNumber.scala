package org.igye.logic.graph

trait HasOrderNumber extends Ordered[HasOrderNumber] {
    val orderNumber: Int

    override def compare(that: HasOrderNumber): Int = that match {
        case hon: HasOrderNumber => orderNumber - that.orderNumber
        case _ => ???
    }
}
