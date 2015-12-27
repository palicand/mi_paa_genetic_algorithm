package cz.cvut.fit.palicand.knapsack.algorithms.evolution

import cz.cvut.fit.palicand.knapsack.algorithms._
import cz.cvut.fit.palicand.knapsack.{KnapsackInstance, KnapsackSolution}

import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by palickaa on 18/12/15.
  */

case class KnapsackIndividual(instance: KnapsackInstance, chromozome: IndexedSeq[Int]) extends GAIndividual[Int] {
  private lazy val solution: KnapsackSolution = {
    new KnapsackSolution(instance, chromozome)
  }
  lazy val fitness: Int = {
    solution.prices
  }

  lazy val weight: Int = {
    solution.weights
  }
}

/**
  *
  * @param problemInstance the concrete instance of a Knapsack problem
  * @param initialPopulation how many individuals do we want to generate
  * @param maxGeneration how many generations (i.e. iterations) we want to compute
  * @param mutationChance
  */
case class KnapsackGeneticAlgorithm(problemInstance: KnapsackInstance,
                                      initialPopulation: Int,
                                      maxGeneration: Int,
                                      tournamentSize: Int,
                                      mutationChance: Double) extends GeneticAlgorithm[KnapsackInstance, KnapsackIndividual, KnapsackSolution](problemInstance,
  initialPopulation,
  maxGeneration) with TournamentSelection[KnapsackIndividual] with OnePointCrossover[Int, KnapsackIndividual] {
  def generateRandomVector() : KnapsackIndividual = {
    var intermediateWeight = 0
    toIndividual(problemInstance.items.map { case (weight, price) =>
      if(Random.nextBoolean() && (intermediateWeight + weight <= problemInstance.capacity)) {
        intermediateWeight += weight
        1
      } else {
        0
      }
    })
  }

  override def toSolution(value: KnapsackIndividual): KnapsackSolution = {
    new KnapsackSolution(problemInstance, value.chromozome)
  }

  override def prune(population: Seq[KnapsackIndividual]) : Seq[KnapsackIndividual] = {
    Random.shuffle(population.filter { (individual) =>
      individual.weight <= problemInstance.capacity
    }).sortBy((individual) => individual.fitness)(Ordering[Int].reverse).take(initialPopulation)
  }

  override def toIndividual(chromozome: IndexedSeq[Int]): KnapsackIndividual = {
    new KnapsackIndividual(problemInstance, chromozome)
  }


  @tailrec
  final def mutate(individual: KnapsackIndividual): KnapsackIndividual = {
    if (Random.nextDouble() <= mutationChance) {
      val pos = Random.nextInt(individual.chromozome.length)

      if (individual.chromozome(pos) == 0) {
        val mutated = individual.chromozome.take(pos) ++ IndexedSeq(1) ++ individual.chromozome.drop(pos + 1)
        val mutatedIndividual = toIndividual(mutated.toIndexedSeq)
        if (mutatedIndividual.weight <= problemInstance.capacity) {
          mutatedIndividual
        } else {
          mutate(individual)
        }
      } else {
        toIndividual(individual.chromozome.take(pos) ++ IndexedSeq(0) ++ individual.chromozome.drop(pos + 1))
      }
    } else {
      individual
    }
  }
}
