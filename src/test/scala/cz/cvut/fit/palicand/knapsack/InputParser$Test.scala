package cz.cvut.fit.palicand.knapsack

import org.scalatest.FlatSpec
import org.scalatest._
/**
 * Created by palicka on 17/12/15.
 */
class InputParser$Test extends FlatSpec with Matchers{

  behavior of "InputParser$Test"

  "InputParser" should "parseInput" in {
    val instances = "1 2 100 1 2 2 1" :: "2 2 100 3 4 8 5" :: Nil
    val solutions = "1 2 3 1 1" :: "2 2 9 1 1" :: Nil
    val expected = new KnapsackInstance(1, 2, 100, 3, IndexedSeq((1,2), (2, 1))) ::
      new KnapsackInstance(2, 2, 100, 9, IndexedSeq((3,4), (8, 5))) :: Nil
    val result = InputParser.parseInput(instances, solutions)
    result should equal (expected)

  }

}
