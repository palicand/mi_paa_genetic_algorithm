package cz.cvut.fit.palicand.knapsack

import java.io.File
import java.lang.System

/**
  * Created by palicka on 15/12/15.
  */
object KnapsackEvA {
  def main(args: Array[String]) {
    if (args.length != 4) {
      System.exit(1)
    }
    val inputData = args(1)
    val solution = args(2)
    val config = args(3)
    getFileNames(new File(inputData).listFiles.filter(_.isFile).map(_.getName),
      new File(solution).listFiles.filter(_.isFile).map(_.getName)).map { case (instance, solution) =>
      InputParser.parseInput(scala.io.Source.fromFile(instance).getLines.toSeq,
        scala.io.Source.fromFile(solution).getLines.toSeq)
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
