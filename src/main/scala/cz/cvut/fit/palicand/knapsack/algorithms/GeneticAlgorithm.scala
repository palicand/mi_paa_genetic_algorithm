package cz.cvut.fit.palicand.knapsack.algorithms

import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by palickaa on 18/12/15.
  */


trait OnePointCrossover[T, IndividualType <: GAIndividual[T]] {

  def toIndividual(head: IndexedSeq[T]): IndividualType

  def crossover(parent1: IndividualType, parent2: IndividualType): IndividualType = {
    val point = Random.nextInt(parent1.chromozome.length)
    val parts1 = parent1.chromozome.splitAt(point)
    val parts2 = parent1.chromozome.splitAt(point)


    toIndividual(Random.shuffle((parts1._1 ++ parts2._2) ::
      (parts2._1 ++ parts1._2) :: Nil).head)
  }

}

trait TournamentSelection[IndividualType <: GAIndividual[_]] {

  val tournamentSize: Int

  def selectParent(population: Seq[IndividualType]): IndividualType = {
    @tailrec
    def selectParentRec(recPopulation: IndexedSeq[IndividualType],
                        best: IndividualType, round: Int): IndividualType = {
      if (round == tournamentSize) {
        return best
      }
      val candidate = recPopulation(Random.nextInt(recPopulation.length))
      selectParentRec(recPopulation, if (candidate.fitness > best.fitness) candidate else best,
      round + 1)
    }
    selectParentRec(population.toIndexedSeq,
      population.toIndexedSeq(Random.nextInt(population.length)),
      1)
  }
}

trait GAIndividual[U] {
  val chromozome: IndexedSeq[U]
  val fitness: Int
}

abstract class GeneticAlgorithm[InstanceType, IndividualType <: GAIndividual[_], SolutionType](problemInstance: InstanceType,
                                                            initialPopulation: Int,
                                                            maxGeneration: Int) {

  def generateRandomVector(): IndividualType

  def toSolution(value: IndividualType): SolutionType

  def crossover(parent1: IndividualType, parent2: IndividualType): IndividualType

  def mutate(individual: IndividualType): IndividualType

  def selectParent(population: Seq[IndividualType]): IndividualType

  def prune(population: Seq[IndividualType]) : Seq[IndividualType]

  @tailrec
  final def runOnGeneration(generation: Int, bestSolution: IndividualType,
                            population: Seq[IndividualType]): IndividualType = {
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
      if (bestSolution.fitness > bestChildSolution.fitness) bestSolution else bestChildSolution,
      pruned)
  }

  def solve(): SolutionType = {
    val population = (0 until initialPopulation).map { (_) =>
      generateRandomVector()
    }
    toSolution(runOnGeneration(0, getTop(population), population))
  }

  def getTop(list: Seq[IndividualType]): IndividualType = {
    list.reduceLeft((left, right) => {
      if (left.fitness > right.fitness) {
        left
      } else {
        right
      }
    })
  }
}
