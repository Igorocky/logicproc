package org.igye.logic

import org.igye.logic.LogicOperators.!

class PredicateStorage {
    private var knownTrueStatements = List[Predicate]()
    private var knownFalseStatements = List[Predicate]()

    def getTrueStatements = knownTrueStatements
    def getFalseStatements = knownFalseStatements

    private def save(stmt: Predicate, isTrue: Boolean): Unit = {
        val (norm, inverted) = normalize(stmt)
        val isTrueNorm = if (inverted) !isTrue else isTrue
        val contradictsTo = contradicts(norm, isTrue)
        if (contradictsTo.isDefined) {
            throw new IllegalArgumentException(s"statement [$stmt is $isTrue] contradicts to [${contradictsTo.get} is ${!isTrueNorm}]")
        }
        if (isTrueNorm) {
            knownTrueStatements ::= norm
        } else {
            knownFalseStatements ::= norm
        }
    }

    def saveTrue(pr: Predicate): Unit = {
        save(pr, true)
    }

    def saveFalse(pr: Predicate): Unit = {
        save(pr, false)
    }

    def save(pr: Predicate): Unit = saveTrue(pr)

    def getTrueOrFalse(inputPr: Predicate): Option[Boolean] = {
        val (normalizedPr, invertRes) = normalize(inputPr)
        knownTrueStatements.foreach{stmt=>
            if (stmt == normalizedPr) {
                return Some(!invertRes)
            }
        }
        knownFalseStatements.foreach{stmt=>
            if (stmt == normalizedPr) {
                return Some(invertRes)
            }
        }
        None
    }

    private def normalize(isStmt: Predicate, inverted: Boolean = false): (Predicate, Boolean/*inverted*/) = isStmt match {
        case !(p) => normalize(p, !inverted)
        case p => (isStmt, inverted)
    }

    private def contradicts(normalizedStmt: Predicate, isTrue: Boolean): Option[Predicate] = {
        if (isTrue) {
            knownFalseStatements.find(_ == normalizedStmt)
        } else {
            knownTrueStatements.find(_ == normalizedStmt)
        }
    }
}
