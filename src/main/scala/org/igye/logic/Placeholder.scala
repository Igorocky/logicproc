package org.igye.logic

case class Placeholder(name: String) extends Predicate(Nil) {

    override def copy(orderedChildren: List[Predicate]): Predicate = Placeholder(name)

    override def toString: String = s"{$name}"
}
