package org.igye.logic

case class Substitution(map: Map[Predicate, Predicate], parent: Option[Substitution]) {
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

    def replaceValues(otherSubs: Substitution): Substitution = {
        Substitution(flattenMap.map{
            case (k,v) =>
//                println("other = " + otherSubs.flattenMap)
//                println((k,v))
                (k, otherSubs.get(v).get)
        }, None)
    }

    def concat(otherSubs: Substitution): Substitution = {
        Substitution(flattenMap, Some(otherSubs))
    }

    override def toString: String = s"Sub(" +
        s"map =    $map, " +
        s"parent = $parent)"
}
