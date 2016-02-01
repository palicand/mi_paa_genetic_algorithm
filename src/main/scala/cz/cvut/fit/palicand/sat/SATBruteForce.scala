package cz.cvut.fit.palicand.sat

import cz.cvut.fit.palicand.sat.utils.Utils

/**
  * Created by palickaa on 18/01/16.
  */
object SATBruteForce {
  def solve(instance: SATInstance): SATSolution = {
    Utils.genBinary(instance.numberOfVariables).map((binSeq) => new SATSolution(instance, binSeq.toIndexedSeq)).maxBy { (solution) =>
      if (solution.satisfied) {
        solution.value
      } else {
        0
      }
    }
  }
}
