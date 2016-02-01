package cz.cvut.fit.palicand.sat

/**
  * Created by palickaa on 10/01/16.
  */

case class SATInstance(numberOfVariables: Int, formula: Seq[Seq[Int]],
                       weights: Seq[Int], controlValue: Int) {

}
