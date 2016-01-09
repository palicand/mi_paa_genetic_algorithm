package cz.cvut.fit.palicand.knapsack.algorithms

import com.typesafe.scalalogging.LazyLogging

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


    Random.shuffle(toIndividual(parts1._1 ++ parts2._2) ::
      toIndividual(parts2._1 ++ parts1._2) :: Nil).head
  }

  def getTop(list: Seq[IndividualType]): IndividualType
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
                                                                                               maxGeneration: Int,
                                                                                               maxEqual: Int) extends LazyLogging {

  def generateRandomVector(): IndividualType

  def toSolution(value: IndividualType): SolutionType

  def crossover(parent1: IndividualType, parent2: IndividualType): IndividualType

  def mutate(individual: IndividualType): IndividualType

  def selectParent(population: Seq[IndividualType]): IndividualType

  def prune(population: Seq[IndividualType]): Seq[IndividualType]

  def selectBreedingPopulation(population: Seq[IndividualType]) : Seq[IndividualType]

  @tailrec
  final def runOnGeneration(generation: Int, bestSolutions: Seq[IndividualType],
                            population: Seq[IndividualType]): IndividualType = {
    if (generation == maxGeneration || generation > maxEqual && bestSolutions.take(maxEqual).forall { (individual) => individual.fitness == bestSolutions.head.fitness }) {
      return bestSolutions.head
    }
    val selectedForBreeding = selectBreedingPopulation(population)
    val children = selectedForBreeding.map { (parent) =>
      crossover(parent, selectParent(selectedForBreeding))
    }
    val pruned = prune(population ++ children).map(mutate)
    val bestChildSolution = getTop(pruned)
    val bestSoFar = if (bestSolutions.head.fitness > bestChildSolution.fitness) bestSolutions.head else bestChildSolution
    logger.info(s"Generation $generation - ${bestChildSolution.fitness} ")
    runOnGeneration(generation + 1,
      bestSoFar +: bestSolutions,
      pruned)
  }

  def solve(): SolutionType = {
    val population = (0 until initialPopulation).map { (_) =>
      generateRandomVector()
    }
    toSolution(runOnGeneration(0, getTop(population) :: Nil, population))
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
