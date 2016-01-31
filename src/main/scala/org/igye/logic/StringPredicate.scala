package org.igye.logic

case class StringPredicate(str: String) extends Predicate(Nil) {

    override def copy(orderedChildren: List[Predicate]): Predicate = StringPredicate(str)

    override def toString: String = str
}
