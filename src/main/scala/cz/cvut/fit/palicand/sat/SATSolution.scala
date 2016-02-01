package cz.cvut.fit.palicand.sat

import com.typesafe.scalalogging.LazyLogging

/**
  * Created by palickaa on 10/01/16.
  */
case class SATSolution(instance: SATInstance, assignment: IndexedSeq[Boolean]) extends LazyLogging {
  lazy val satisfied: Boolean = {
    clausules.foldLeft(true) { (resultSoFar, clausule) =>
      if (!resultSoFar) {
        false
      } else {
        resultSoFar && clausule
      }
    }
  }

  lazy val value: Int = {
    instance.weights.zip(assignment).foldLeft(0){ (tempRes, currentVal) =>
      currentVal match {
        case (weight, concreteAssignment) => {
          if(concreteAssignment) {
            tempRes + weight
          } else {
            tempRes
          }
        }
      }
    }
  }

  lazy val clausules: IndexedSeq[Boolean] = {
    instance.formula.map(_.foldLeft(false) {(clausuleSoFar, value) =>
      if(clausuleSoFar) {
        true
      } else {
        if(value < 0) {
          clausuleSoFar || !assignment(-value - 1)
        } else {
          clausuleSoFar || assignment(value - 1)
        }
      }
    })
  }.toIndexedSeq
}
