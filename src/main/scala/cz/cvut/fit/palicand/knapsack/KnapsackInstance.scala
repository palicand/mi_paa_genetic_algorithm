package cz.cvut.fit.palicand.knapsack

/**
 * Created by palicka on 15/12/15.
 */
case class KnapsackInstance(id: Int, size: Int,  capacity: Int,
                        controlValue: Int,
                        items: IndexedSeq[(Int, Int)]) {

}
