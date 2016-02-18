package org.igye.logic

import org.igye.logic.LogicOperators.{&, or, toDnf}
import org.igye.logic.LogicalOperationsOnPredicate.predicateToLogicalOperationsOnPredicate

object LogicalExpressions {
    def applyRule(rule: Rule)(implicit predicateStorage: PredicateStorage): List[Predicate] = {
        disjToList(toDnf(rule.condition)).flatMap(conj => applyRulePriv(conj --> rule.result))
    }

    private def applyRulePriv(rule: Rule)(implicit predicateStorage: PredicateStorage): List[Predicate] = {
        val predicatesOfConj = conjToList(rule.condition)
        val substitutions = predicatesOfConj.tail.foldLeft{
            predicateStorage.getTrueStatements.flatMap(createSubstitution(predicatesOfConj.head, _))
        }{
            case (substitutions, predicate) =>
                substitutions.flatMap{sub=>
                    predicateStorage.getTrueStatements.flatMap(createSubstitution(predicate, _, Some(sub)))
                }
        }
        substitutions.map(applySubstitution(rule.result, _))
    }

    def applySubstitution(pr: Predicate, sub: Substitution): Predicate = {
        sub.get(pr).getOrElse{
            pr.copy(pr.orderedChildren.map{
                case pr: Predicate => applySubstitution(pr, sub)
            })
        }
    }

    def eval(exp: Predicate)(implicit predicateStorage: PredicateStorage): Option[Boolean] = {
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

    def findSubStructures(whereToSearch: Predicate, pattern: Predicate): List[(Predicate, Substitution)] = {
        createSubstitution(pattern, whereToSearch).map(s => List((whereToSearch, s))).getOrElse(Nil):::
            whereToSearch.orderedChildren.flatMap(findSubStructures(_, pattern))
    }

    def createSubstitution(fromPr: Predicate, toPr: Predicate,
                           parent: Option[Substitution] = None): Option[Substitution] = {
        (fromPr, toPr) match {
            case (from: Placeholder, to: Placeholder) =>
                createSubstBetweenPlaceholderAndPredicate(from, to, parent)
            case (from: Placeholder, to: Predicate) =>
                createSubstBetweenPlaceholderAndPredicate(from, to, parent)
            case (from: Predicate, to: Placeholder) =>
                createSubstBetweenPlaceholderAndPredicate(from, to, parent)
            case (fromPr: Predicate, toPr: Predicate) =>
                if (fromPr.orderedChildren.length != toPr.orderedChildren.length ||
                    fromPr.getClass != toPr.getClass ||
                    fromPr.orderedChildren.isEmpty && fromPr != toPr) {
                    None
                } else if (fromPr == toPr) {
                    Some(Substitution(map = Map(), parent))
                } else {
                    fromPr.orderedChildren.zip(toPr.orderedChildren).foldLeft(parent){
                        case (soFarRes, (from, to)) =>
                            val res = createSubstitution(from, to, soFarRes)
                            if (res.isEmpty) {
                                return None
                            } else {
                                res
                            }
                    }
                }
        }
    }

    private def createSubstBetweenPlaceholderAndPredicate(
                                                             from: Predicate,
                                                             to: Predicate,
                                                             soFarRes: Option[Substitution]
                                                         ): Option[Substitution] = {
        if (soFarRes.exists(_.contradicts(from, to))) {
            None
        } else {
            Some(Substitution(map = Map(from -> to), parent = soFarRes))
        }
    }

    private def evalConj(conj: Predicate)(implicit predicateStorage: PredicateStorage): Option[Boolean] = {
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
