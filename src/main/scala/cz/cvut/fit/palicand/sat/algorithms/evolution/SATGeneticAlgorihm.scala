package cz.cvut.fit.palicand.sat.algorithms.evolution

import cz.cvut.fit.palicand.algorithms.{GAIndividual, GeneticAlgorithm, OnePointCrossover, TournamentSelection}
import cz.cvut.fit.palicand.knapsack.algorithms.evolution.KnapsackIndividual
import cz.cvut.fit.palicand.sat.{SATInstance, SATSolution}
import sun.security.util.Length

import scala.annotation.tailrec
import scala.util.Random

/**
  * Created by palickaa on 19/01/16.
  */


case class SATGeneticAlgorihm(problemInstance: SATInstance, initialPopulation: Int,
                         maxGeneration: Int,
                         tournamentSize: Int,
                         mutationChance: Double,
                         maxEqual: Int) extends GeneticAlgorithm[SATInstance, SATIndividual, SATSolution](problemInstance,
  initialPopulation,
  maxGeneration, maxEqual) with TournamentSelection[SATIndividual] with OnePointCrossover[Boolean, SATIndividual] {

  override def generateRandomVector(): SATIndividual = {
    @tailrec
    def generateBinaryVector(partialVector: IndexedSeq[Boolean]) : IndexedSeq[Boolean] = {
      if(partialVector.length == problemInstance.numberOfVariables) {
        partialVector
      } else {
        generateBinaryVector(Random.nextBoolean() +: partialVector)
      }
    }

    new SATIndividual(problemInstance,
                      generateBinaryVector(IndexedSeq.empty[Boolean]))
  }

  override def toSolution(value: SATIndividual): SATSolution = {
    value.solution
  }

  override def mutate(individual: SATIndividual): SATIndividual = {
    val filtered = individual.chromozome.zipWithIndex.map { case (value, index) =>
        new SATIndividual(individual.instance, individual.chromozome.updated(index, !value))
    }.filter(_.numberOfSatisfiedClausules >= individual.numberOfSatisfiedClausules)
    if(filtered.nonEmpty) {
      filtered.groupBy(_.numberOfSatisfiedClausules - individual.numberOfSatisfiedClausules).maxBy { case (key, v) => key }._2.maxBy(_.fitness)
    } else {
      individual
    }
  }

  override def prune(population: Seq[SATIndividual]): Seq[SATIndividual] = {
    val filtered = population.filter(_.valid).sortBy {_.fitness}(Ordering[Int].reverse)
    val top = filtered.take(initialPopulation / 2) ++ filtered.takeRight(initialPopulation / 2)
    top ++ (top.length to initialPopulation).map(_=>generateRandomVector())
  }

  override def selectBreedingPopulation(population: Seq[SATIndividual]): Seq[SATIndividual] = {
    population
  }

  override def toIndividual(chromozome: IndexedSeq[Boolean]): SATIndividual = {
    new SATIndividual(problemInstance, chromozome)
  }

}

case class SATIndividual(instance: SATInstance, chromozome: IndexedSeq[Boolean]) extends GAIndividual[Boolean] {
  lazy val solution = new SATSolution(instance, chromozome)
  lazy val fitness = {
    if(!valid) {
      numberOfSatisfiedClausules.toInt * solution.value
    } else {
      numberOfSatisfiedClausules.toInt * (solution.value + instance.weights.sum)
    }
  }
  lazy val numberOfSatisfiedClausules = solution.clausules.count { (clausule) => clausule }
  lazy val valid = solution.satisfied
}
