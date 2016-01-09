package cz.cvut.fit.palicand.knapsack

import java.io.File
import java.lang.System

import com.typesafe.scalalogging.{LazyLogging, Logger}
import cz.cvut.fit.palicand.knapsack.algorithms.evolution.KnapsackGeneticAlgorithm

import scala.util.Random

/**
  * Created by palicka on 15/12/15.
  */
object KnapsackEvA extends LazyLogging {
  def main(args: Array[String]) {
    if (args.length != 3) {
      System.exit(1)
    }
    Random.setSeed(System.currentTimeMillis())
    val inputData = args(0)
    val solution = args(1)
    val config = args(0)
    val instances = getFileNames(new File(inputData).listFiles.filter(_.isFile).map(_.getName),
      new File(solution).listFiles.filter(_.isFile).map(_.getName)).map {
      case (instanceFile, solutionFile) =>
        val parsed = InputParser.parseInput(scala.io.Source.fromFile(inputData + "/" + instanceFile).getLines.toSeq,
          scala.io.Source.fromFile(solution + "/" + solutionFile).getLines.toSeq)
        parsed.head.size -> parsed
    }(collection.breakOut): scala.collection.SortedMap[Int, Seq[KnapsackInstance]]
    instances.map { case (size, instanceCollection) =>
      logger.info(s"Instance size: ${size}")
      val solutions = instanceCollection.toList.map { (instance) =>
        logger.info(s"Start: ${instance.id}")
        val start = System.nanoTime()
        val algorithm = new KnapsackGeneticAlgorithm(instance, 100 * instance.size, 10 * instance.size, 3, 0.01, 2)
        val solution = algorithm.solve()
        val runTime = System.nanoTime() - start
        logger.info(s"Total: ${instance.id} ${instance.controlValue} ${solution.prices} $runTime")
        solution
      }
      size -> solutions
    }

  }

  def getFileNames(instanceFiles: Seq[String], solutionFiles: Seq[String]): Iterable[(String, String)] = {
    val sortedInstance = instanceFiles.sortWith(compareFileNames)
    val sortedSol = solutionFiles.sortWith(compareFileNames)
    sortedInstance.zip(sortedSol)
  }

  def compareFileNames(left: String, right: String): Boolean = {
    val filePattern = """.*?([0-9]+).*?""".r
    val leftSize = left match {
      case filePattern(size) => size.toInt
    }
    val rightSize = right match {
      case filePattern(size) => size.toInt
    }
    leftSize < rightSize
  }
}
