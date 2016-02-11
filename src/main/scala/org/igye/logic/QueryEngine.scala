package org.igye.logic

import org.igye.logic.LogicalExpressions.{applySubstitution, createSubstitution}

object QueryEngine {
    def query(pr: Predicate)
             (implicit predicateStorage: PredicateStorage, ruleStorage: RuleStorage): List[Substitution] = {
        query(Set(pr))
    }

    def query(querySet: Set[Predicate], collectedSubst: Option[Substitution] = None)
             (implicit predicateStorage: PredicateStorage, ruleStorage: RuleStorage): List[Substitution] = {

        val halfRes = querySingle(querySet.head, collectedSubst)

        if (querySet.size == 1) {
            halfRes
        } else {
            halfRes.flatMap(s => query(querySet.tail, Some(s)))
        }
    }

    def querySingle(queryPr: Predicate, collectedSubst: Option[Substitution])
             (implicit predicateStorage: PredicateStorage, ruleStorage: RuleStorage): List[Substitution] = {
        val resFromPredicateStorage = predicateStorage.getTrueStatements.flatMap(createSubstitution(queryPr, _, collectedSubst))
        val queryPrWithSubs = if (collectedSubst.isDefined) applySubstitution(queryPr, collectedSubst.get) else queryPr
        val resFromRuleStorage = createEquivalentQueries(queryPrWithSubs).flatMap(query(_, collectedSubst)).map(sub=>
            Substitution(from = sub.from, to = sub.to, map = sub.flattenMap, parent = collectedSubst)
        )

        resFromPredicateStorage:::resFromRuleStorage
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
