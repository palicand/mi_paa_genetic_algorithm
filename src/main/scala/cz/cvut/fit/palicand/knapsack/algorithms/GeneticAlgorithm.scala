package cz.cvut.fit.palicand.knapsack.algorithms

import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by palickaa on 18/12/15.
  */


trait OnePointCrossover {

  def crossover(parent1: IndexedSeq[Int], parent2: IndexedSeq[Int]): IndexedSeq[Int] = {
    val point = Random.nextInt(parent1.length)
    val parts1 = parent1.splitAt(point)
    val parts2 = parent2.splitAt(point)
    Random.shuffle((parts1._1 ++ parts2._2) ::
      (parts2._1 ++ parts1._2) :: Nil).head
  }

}

trait TournamentSelection {

  val tournamentSize: Int

  def fitness(individual: IndexedSeq[Int]): Int

  def selectParent(population: Seq[IndexedSeq[Int]]): IndexedSeq[Int] = {
    @tailrec
    def selectParentRec(recPopulation: IndexedSeq[IndexedSeq[Int]],
                        best: IndexedSeq[Int], round: Int): IndexedSeq[Int] = {
      if (round == tournamentSize) {
        return best
      }
      val candidate = recPopulation(Random.nextInt(recPopulation.length))
      selectParentRec(recPopulation, if (fitness(candidate) > fitness(best)) candidate else best,
      round + 1)
    }
    selectParentRec(population.toIndexedSeq,
      population.toIndexedSeq(Random.nextInt(population.length)),
      1)
  }
}

trait RandomPrune {
  val initialPopulation: Int
  def prune(population: Seq[IndexedSeq[Int]]) : Seq[IndexedSeq[Int]] = {
    Random.shuffle(population).take(initialPopulation)
  }
}

trait GAIndividual[T <: Ordered[T]] {
  def fitness(): T
}

abstract class GeneticAlgorithm[InstanceType, SolutionType](problemInstance: InstanceType,
                                                            initialPopulation: Int,
                                                            maxGeneration: Int) {

  def generateRandomVector(): IndexedSeq[Int]

  def toSolution(value: IndexedSeq[Int]): SolutionType

  def crossover(parent1: IndexedSeq[Int], parent2: IndexedSeq[Int]): IndexedSeq[Int]

  def mutate(individual: IndexedSeq[Int]): IndexedSeq[Int]

  def selectParent(population: Seq[IndexedSeq[Int]]): IndexedSeq[Int]

  def prune(population: Seq[IndexedSeq[Int]]) : Seq[IndexedSeq[Int]]

  @tailrec
  final def runOnGeneration(generation: Int, bestSolution: IndexedSeq[Int],
                            population: Seq[IndexedSeq[Int]]): IndexedSeq[Int] = {
    if (generation == maxGeneration) {
      return bestSolution
    }

    val children = population.indices.map { (child) =>
      val child = crossover(selectParent(population), selectParent(population))
      mutate(child)
    }
    val pruned = prune(population ++ children)
    val bestChildSolution = getTop(pruned)
    runOnGeneration(generation + 1,
      if (fitness(bestSolution) > fitness(bestChildSolution)) bestSolution else bestChildSolution,
      pruned)
  }

  def solve(): SolutionType = {
    val population = (0 until initialPopulation).map { (_) =>
      generateRandomVector()
    }
    toSolution(runOnGeneration(0, getTop(population), population))
  }

  def fitness(individual: IndexedSeq[Int]): Int

  def getTop(list: Seq[IndexedSeq[Int]]): IndexedSeq[Int] = {
    list.reduce((left, right) => {
      if (fitness(left) > fitness(right)) {
        left
      } else {
        right
      }
    })
  }
}
