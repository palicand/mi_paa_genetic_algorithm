package cz.cvut.fit.palicand.knapsack

import cz.cvut.fit.palicand.sat.{SATSolution, SATInstance}
import org.scalatest.{Matchers, FlatSpec, BeforeAndAfter}

/**
  * Created by palickaa on 10/01/16.
  */
class SATSolutionTest extends FlatSpec with Matchers with BeforeAndAfter {

  var instance : SATInstance = _

  before {
    instance = new SATInstance(3, (0 :: 1 :: 2 :: Nil) ::
      (0 :: -1 :: -2 :: Nil) :: (-0 :: 1 :: 2 :: Nil) :: Nil,
      1 :: 3 :: 2 :: Nil, 10)
  }
  it should "return true when the formula is satisfied " in {
    val assignments = (true :: true :: true :: Nil) :: (true :: false :: true :: Nil) :: Nil
    assignments.foreach {
      (assignment) =>
      val solution = SATSolution(instance,
        assignment.toIndexedSeq)
      solution.satisfied should be(true)
    }
  }

  it should "return false when the formula is satisfied " in {
    val solution = SATSolution(instance,
      (false :: false :: false :: Nil).toIndexedSeq)
    solution.satisfied should be (false)
  }

}
