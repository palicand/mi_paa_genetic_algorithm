package cz.cvut.fit.palicand.knapsack

/**
 * Created by palicka on 17/12/15.
 */
object InputParser {
  val inputLineRegex = """([0-9]+) ([0-9]+) ([0-9]+) (.*?)""".r
  val solutionRegex = """([0-9]+) [0-9]+ ([0-9]+).*?""".r
  val itemsRegex = """([0-9]+) ([0-9])+"""
  def parseInput(instances: Seq[String], solutions: Seq[String]): Seq[KnapsackInstance] = {
    val joined = instances.zip(solutions)
    joined.map { joined =>
      val inputLine = joined._1
      val solLine = joined._2
      inputLine match {
        case inputLineRegex(id, size, capacity, items) => {
          solLine match {
            case solutionRegex(solId, value) => {
              val pairs = items.split(" ").grouped(2).map {
                pair =>
                  (pair(0).toInt, pair(1).toInt)
              }.toIndexedSeq
              new KnapsackInstance(id.toInt, size.toInt, capacity.toInt, value.toInt, pairs)
            }
          }
        }
      }
    }
  }
}
