package cz.cvut.fit.palicand.sat.utils

import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by palickaa on 19/01/16.
  */
class Utils$Test extends FlatSpec with Matchers {

  behavior of "Utils$Test"

  it should "generateAllBinarySeq" in {
    Utils.genBinary(3) should equal ((0 +: 0 +: 0 +: Nil) +:
      (0 +: 0 +: 1 +: Nil) +: (0 +: 1 +: 0 +: Nil) +:
      (0 +: 1 +: 1 +: Nil) +: (1 +: 0 +: 0 +: Nil) +:
      (1+: 0 +: 1 +: Nil) +: (1 +: 1 +: 0 +: Nil) +:
      (1 +: 1 +: 1 +: Nil) +: Nil)
  }

}
