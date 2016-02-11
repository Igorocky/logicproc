package org.igye.logic

case class Substitution(from: Predicate, to: Predicate, map: Map[Predicate, Predicate], parent: Option[Substitution]) {
    lazy val flattenMap: Map[Predicate, Predicate] = map ++ parent.map(_.flattenMap).getOrElse(Map())

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

    def reverse = Substitution(
        from = to,
        to = from,
        map = flattenMap.toList.map{case (k,v) => (v,k)}.toMap,
        None
    )

    override def toString: String = s"Sub(" +
        s"from =   $from, " +
        s"to =     $to, " +
        s"map =    $map, " +
        s"parent = $parent)"
}
