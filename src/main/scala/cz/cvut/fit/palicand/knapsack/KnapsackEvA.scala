package cz.cvut.fit.palicand.knapsack

import java.io.File
import java.lang.System

import cz.cvut.fit.palicand.knapsack.algorithms.evolution.KnapsackGeneticAlgorithm

/**
  * Created by palicka on 15/12/15.
  */
object KnapsackEvA {
  def main(args: Array[String]) {
    if (args.length != 3) {
      System.exit(1)
    }
    val inputData = args(0)
    val solution = args(1)
    val config = args(0)
    val instances = getFileNames(new File(inputData).listFiles.filter(_.isFile).map(_.getName),
      new File(solution).listFiles.filter(_.isFile).map(_.getName)).flatMap {
      case (instanceFile, solutionFile) =>
        InputParser.parseInput(scala.io.Source.fromFile(inputData + "/" + instanceFile).getLines.toSeq,
          scala.io.Source.fromFile(solution + "/" + solutionFile).getLines.toSeq)
    }
    val solved = instances.map { (instance) =>
      val algorithm = new KnapsackGeneticAlgorithm(instance, 50 * instance.size, 200, 2, 0.04)
      val solution = algorithm.solve()
      println((solution.instance.controlValue - solution.prices).toDouble / solution.instance.controlValue.toDouble)
      solution
    }

    solved.foreach { (solution) => {
      println((solution.instance.controlValue - solution.prices).toDouble / solution.instance.controlValue.toDouble)
    }}


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
