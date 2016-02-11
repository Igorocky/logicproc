package org.igye.logic

import org.igye.logic.LogicalExpressions.{applySubstitution, createSubstitution}

object QueryEngine {
    def query(querySet: Set[Predicate], initialSubst: Option[Substitution] = None)
             (implicit predicateStorage: PredicateStorage): List[Substitution] = {

        val query2 = (if (initialSubst.isDefined) {
            querySet.map(applySubstitution(_, initialSubst.get))
        } else {
            querySet
        })

        val halfRes = querySingle(querySet.head, initialSubst)

        if (querySet.size == 1) {
            halfRes
        } else {
            halfRes.flatMap(s => query(querySet.tail, Some(s)))
        }
    }

    def querySingle(queryPr: Predicate, initialSubst: Option[Substitution] = None)
             (implicit predicateStorage: PredicateStorage): List[Substitution] = {
        predicateStorage.getTrueStatements.flatMap(createSubstitution(queryPr, _, initialSubst))
    }

    def createEquivalentQueries(queryPr: Predicate)(implicit ruleStorage: RuleStorage): List[Set[Predicate]] = {
        ruleStorage.getSubRules
            .map(subRule => (createSubstitution(queryPr, subRule.result), subRule))
            .filter(_._1.isDefined)
            .map{
                case (substOpt, subRule) =>
                    val reverseSubst = substOpt.get.reverse
                    subRule.conjSet.map(applySubstitution(_, reverseSubst))
            }
    }
}
