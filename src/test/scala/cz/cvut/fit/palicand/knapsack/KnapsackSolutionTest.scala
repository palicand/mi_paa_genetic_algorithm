package cz.cvut.fit.palicand.knapsack

import org.scalatest.{Matchers, BeforeAndAfter, FlatSpec}

/**
  * Created by palickaa on 18/12/15.
  */
class KnapsackSolutionTest extends FlatSpec with BeforeAndAfter with Matchers{

  behavior of "KnapsackSolutionTest"
  var instance : KnapsackInstance= _
  before {
    instance = KnapsackInstance(1, 2, 10, 10, IndexedSeq((1, 2), (2, 3)))
  }

  it should "populate fields items when created" in {
    val solution = KnapsackSolution(instance, 1 :: 0 :: Nil)
    solution.items should equal((1, 2, 1) :: (2, 3, 0) :: Nil)
  }

  it should "sum weights properly" in {
    val solution = KnapsackSolution(instance, 1 :: 1 :: Nil)
    solution.weights should be (3)

    val solution2 = KnapsackSolution(instance, 0 :: 0 :: Nil)
    solution2.weights should be (0)
  }

  it should "sum prices properly" in {
    val solution = KnapsackSolution(instance, 1 :: 1 :: Nil)
    solution.prices should be (5)

    val solution2 = KnapsackSolution(instance, 0 :: 0 :: Nil)
    solution2.prices should be (0)
  }

}
