package cz.cvut.fit.palicand.sat

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import cz.cvut.fit.palicand.sat.algorithms.evolution.SATGeneticAlgorihm

import scala.util.Random

/**
  * Created by palickaa on 10/01/16.
  */
object SATEvA extends LazyLogging {

  def main(args: Array[String]) {
    if (args.length != 1) {
      System.exit(1)
    }
    Random.setSeed(System.currentTimeMillis())
    val filePattern = "sat3_[0-9]+.in".r
    val metrics = new File(args(0)).listFiles.filter { (file) =>
      file.isFile && (file.getName match {
        case filePattern(_*) => true
        case _ => false
      })}.map(_.getAbsolutePath).map(parseFile).map {(instance) =>
      val bfSol = SATBruteForce.solve(instance)
      logger.info("Solving")
      val start =  System.nanoTime()
      val algorithm = new SATGeneticAlgorihm(new SATInstance(instance.numberOfVariables, instance.formula, instance.weights, bfSol.value),
        instance.numberOfVariables*50,
      10*instance.numberOfVariables, 2, 1.0, 10)
      val sol = algorithm.solve()
      val time = (System.nanoTime() - start) / 1000000
      val error = (sol.instance.controlValue - sol.value).toDouble / sol.instance.controlValue.toDouble
      // logger.info(s"Total:  ${sol.instance.controlValue} ${sol.value} $error $time ${sol.assignment.toString}")
      (error, time)
    }
    val sum = metrics.foldLeft((0.0, 0l)) { case ((leftErr, leftTime), (rightErr, rightTime)) =>
      (leftErr + rightErr , leftTime + rightTime)
    }
    val max = metrics.foldLeft((0.0, 0l)) { case ((leftErr, leftTime), (rightErr, rightTime)) =>
      (leftErr + rightErr , leftTime + rightTime)
    }
    logger.info(s"avg error: ${sum._1 / metrics.length}, avg time: ${sum._2.toDouble / metrics.length}")
    logger.info(s"max error: ${max._1}, max time: ${max._1}")

  }

  def parseFile(fileName: String): SATInstance = {
    var weights = Seq.empty[Int]
    var formula = Seq.empty[Seq[Int]]
    var numberOfVariables = 0
    var knownSol = 0
    scala.io.Source.fromFile(fileName).getLines().foreach { (line) =>
      val knownSolutionPattern = "c Known solution cost is ([0-9]+) ".r
      val variablesPattern = "p cnf ([0-9]+) ([0-9]+)".r
      val weightPattern = "w ((?:[0-9]+ ?)+)".r
      val formulaPattern = "(-?[0-9]+) (-?[0-9]+) (-?[0-9]+) (-?[0-9]+)".r
      line match  {
        case knownSolutionPattern(sol) => knownSol = sol.toInt
        case variablesPattern(numVar, _) => numberOfVariables = numVar.toInt
        case weightPattern(weightsString) =>
          weights = weightsString.split(" ").map(_.toInt)
        case formulaPattern(var1, var2, var3, _) => formula = formula :+ (var1.toInt :: var2.toInt :: var3.toInt :: Nil)
        case _ => None
      }
    }
    new SATInstance(numberOfVariables, formula, weights, knownSol)
  }


}
