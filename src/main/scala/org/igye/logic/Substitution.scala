package org.igye.logic

case class Substitution(from: Predicate, to: Predicate, map: Map[Predicate, Predicate], parent: Option[Substitution]) {
    def contradicts(key: Predicate, value: Predicate): Boolean = {
        val res = get(key)
        !res.isEmpty && res.get != value
    }

    def get(key: Predicate): Option[Predicate] = {
        if (map.contains(key)) {
            Some(map(key))
        } else {
            parent.flatMap(_.get(key))
        }
    }

    def flattenMap: Map[Predicate, Predicate] = {
        map ++ parent.map(_.flattenMap).getOrElse(Map())
    }

    override def toString: String = s"Sub(" +
        s"from =   $from, " +
        s"to =     $to, " +
        s"map =    $map, " +
        s"parent = $parent)"
}
