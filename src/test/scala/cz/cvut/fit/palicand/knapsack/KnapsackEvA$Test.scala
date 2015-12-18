package cz.cvut.fit.palicand.knapsack

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by palicka on 17/12/15.
 */
class KnapsackEvA$Test extends FlatSpec with Matchers {

  behavior of "KnapsackEvA"

  it should "compareFileNames" in {
    KnapsackEvA.compareFileNames("knap_1.inst.dat", "knap_2.inst.dat") should be (true)
    KnapsackEvA.compareFileNames("knap_785.inst.dat", "knap_2.inst.dat") should be (false)
    KnapsackEvA.compareFileNames("knap_2.inst.dat", "knap_785.inst.dat") should be (true)
    KnapsackEvA.compareFileNames("knap_1.inst.dat", "knap_1.inst.dat") should be (false)
  }

  it should "sort file names" in {
    KnapsackEvA.getFileNames("knap_2.inst.dat" :: "knap_40.inst.dat" :: "knap_5.inst.dat"
    :: "knap_1.inst.dat" :: Nil, "knap_5.sol.dat" :: "knap_2.sol.dat" :: "knap_1.sol.dat"
      :: "knap_40.sol.dat" :: Nil) should equal (("knap_1.inst.dat", "knap_1.sol.dat") ::
      ("knap_2.inst.dat", "knap_2.sol.dat") ::
      ("knap_5.inst.dat", "knap_5.sol.dat") :: ("knap_40.inst.dat", "knap_40.sol.dat") :: Nil)
  }

}
