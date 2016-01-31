package org.igye.logic

import org.igye.logic.LogicOperators.{&, or, toDnf}
import org.igye.logic.LogicalOperationsOnPredicate._

class LogicalExpressions(predicateStorage: PredicateStorage) {
    def applyRule(rule: Rule): List[Predicate] = {
        disjToList(toDnf(rule.condition)).flatMap(conj => applySubRule(Rule(conj, rule.result)))
    }

    private def applySubRule(rule: Rule): List[Predicate] = {
        val subConjList = conjToList(rule.condition)
        val substitutions = subConjList.tail.foldLeft{
            (predicateStorage.getTrueStatements:::
                predicateStorage.getFalseStatements.map(!_)).flatMap(createSubstitutions(subConjList.head, _))
        }{
            case (mappings, conj) =>
                mappings.flatMap{map=>
                    (predicateStorage.getTrueStatements:::
                        predicateStorage.getFalseStatements.map(!_)).flatMap(createSubstitutions(conj, _, map))
                }
        }
        substitutions.map(replacePlaceholders(rule.result, _))
    }

    def replacePlaceholders(pattern: Predicate, subs: Map[Placeholder, Predicate]): Predicate = {
        pattern.copy(pattern.orderedChildren.map{
            case ph: Placeholder => subs(ph)
            case pr: Predicate => replacePlaceholders(pr, subs)
        })
    }

    def eval(exp: Predicate): Option[Boolean] = {
        var hasNone = false
        disjToList(toDnf(exp)).foreach{conj=>
            evalConj(conj) match {
                case s @ Some(true) => return s
                case None => hasNone = true
                case Some(false) =>
            }
        }
        if (hasNone) {
            None
        } else {
            Some(false)
        }
    }

    def createSubstitutions(pattern: Predicate, target: Predicate,
                            existingMapping: Map[Placeholder, Predicate] = Map()): Option[Map[Placeholder, Predicate]] = {
        if (pattern.orderedChildren.length != target.orderedChildren.length ||
                pattern.getClass != target.getClass ||
            pattern.orderedChildren.isEmpty && pattern != target) {
            None
        } else {
            Some(pattern.orderedChildren.zip(target.orderedChildren).foldLeft(existingMapping){
                case (soFarRes, currPair) =>
                    currPair match {
                        case (ph: Placeholder, pr: Predicate) =>
                            if (soFarRes.contains(ph) && soFarRes(ph) != pr) {
                                return None
                            } else {
                                soFarRes + (ph -> pr)
                            }
                        case (pattPr: Predicate, targPr: Predicate) =>
                            val childRes = createSubstitutions(pattPr, targPr, soFarRes)
                            if (childRes.isEmpty) {
                                return None
                            } else {
                                soFarRes ++ childRes.get
                            }
                    }
            })
        }
    }

    private def evalConj(conj: Predicate): Option[Boolean] = {
        var hasNone = false
        conjToList(conj).foreach{pr=>
            predicateStorage.getTrueOrFalse(pr) match {
                case None => hasNone = true
                case f @ Some(false) => return f
                case Some(true) =>
            }
        }
        if (hasNone) {
            None
        } else {
            Some(true)
        }
    }

    protected[logic] def conjToList(ands: Predicate): List[Predicate] = ands match {
        case (l: &) & (r: &) => conjToList(l):::conjToList(r)
        case l & (r: &) => l::conjToList(r)
        case (l: &) & r => conjToList(l):::r::Nil
        case l & r => l::r::Nil
        case p => List(p)
    }

    protected[logic] def disjToList(ors: Predicate): List[Predicate] = ors match {
        case (l: or) or (r: or) => disjToList(l):::disjToList(r)
        case l or (r: or) => l::disjToList(r)
        case (l: or) or r => disjToList(l):::r::Nil
        case l or r => l::r::Nil
        case p => List(p)
    }
}
