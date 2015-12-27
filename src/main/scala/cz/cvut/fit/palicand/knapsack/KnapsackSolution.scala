package cz.cvut.fit.palicand.knapsack

/**
  * Created by palickaa on 18/12/15.
  */
case class KnapsackSolution(instance: KnapsackInstance, solutionVector: Seq[Int]) {
  lazy val items = instance.items.zip(solutionVector).map {
    case (pair, inKnapsack) => {
      (pair._1, pair._2, inKnapsack)
    }
  }

  lazy val prices = items.foldLeft(0) {
    (leftVal, item) => item match {
      case (_, price, inKnapsack) => {
        leftVal + price * inKnapsack
      }
    }
  }

  lazy val weights =  items.foldLeft(0) {
    (leftVal, item) => item match {
      case (weight, _, inKnapsack) => {
        leftVal + weight * inKnapsack
      }
    }
  }


}
