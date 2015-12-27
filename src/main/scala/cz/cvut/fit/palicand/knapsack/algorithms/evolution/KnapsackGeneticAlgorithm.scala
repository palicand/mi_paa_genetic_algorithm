package cz.cvut.fit.palicand.knapsack.algorithms.evolution

import cz.cvut.fit.palicand.knapsack.algorithms.{RandomPrune, TournamentSelection, GeneticAlgorithm, OnePointCrossover}
import cz.cvut.fit.palicand.knapsack.{KnapsackInstance, KnapsackSolution}

import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by palickaa on 18/12/15.
  */

case class EvolutionEntity(instance: KnapsackInstance, chromozome: Seq[Boolean]) extends Ordered [EvolutionEntity]{
  def fitness(): Int = {
    chromozome.zip(instance.items).filter({ case (gene, (_, _)) =>
      gene
    }).foldLeft(0) { (intermediateValue, item) => {
      item match {
        case (_, (_, price)) => {
          intermediateValue + price
        }
      }
    }
    }
  }

  override def compare(that: EvolutionEntity): Int = {
    if(this.fitness == that.fitness) {
      0
    } else if (fitness < that.fitness) {
      -1
    } else {
      1
    }
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
                                      mutationChance: Double) extends GeneticAlgorithm[KnapsackInstance, KnapsackSolution](problemInstance,
  initialPopulation,
  maxGeneration) with TournamentSelection with OnePointCrossover with RandomPrune {
  def generateRandomVector() : IndexedSeq[Int] = {
    var intermediateWeight = 0
    problemInstance.items.map { case (weight, price) =>
      if(Random.nextBoolean() && (intermediateWeight + weight <= problemInstance.capacity)) {
        intermediateWeight += weight
        1
      } else {
        0
      }
    }
  }

  override def toSolution(value: IndexedSeq[Int]): KnapsackSolution = {
    new KnapsackSolution(problemInstance, value)
  }

  override def fitness(individual: IndexedSeq[Int]): Int = {
    toSolution(individual).prices
  }

  override def prune(population: Seq[IndexedSeq[Int]]) : Seq[IndexedSeq[Int]] = {
    Random.shuffle(population.filter { (individual) =>
      toSolution(individual).weights <= problemInstance.capacity
    }).take(initialPopulation)
  }


  @tailrec
  final def mutate(individual: IndexedSeq[Int]): IndexedSeq[Int] = {
    if (Random.nextDouble() <= mutationChance) {
      val pos = Random.nextInt(individual.length)

      if (individual(pos) == 0) {
        val mutated = individual.take(pos) ++ IndexedSeq(1) ++ individual.drop(pos + 1)
        if (toSolution(mutated).weights <= problemInstance.capacity) {
          mutated
        } else {
          mutate(individual)
        }
      } else {
        individual.take(pos) ++ IndexedSeq(0) ++ individual.drop(pos + 1)
      }
    } else {
      individual
    }
  }
}
