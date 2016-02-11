package org.igye.logic

import org.igye.logic.LogicOperators._
import org.igye.logic.LogicalExpressions.{conjToList, disjToList}

class RuleStorage(rules: List[Rule] = Nil) {
    def this(rules: Rule*) = this(rules.toList)

    private var subRules: List[SubRule] = rules.flatMap(createSubRules)

    def getSubRules: List[SubRule] = subRules

    private def createSubRules(rule: Rule) = {
        disjToList(toDnf(rule.condition)).map(conj => SubRule(conjToList(conj).toSet, rule.result, rule))
    }
}
