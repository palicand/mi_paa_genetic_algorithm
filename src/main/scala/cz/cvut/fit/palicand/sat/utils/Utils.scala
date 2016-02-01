package cz.cvut.fit.palicand.sat.utils

/**
  * Created by palickaa on 19/01/16.
  */
object Utils {
  val genBinary: Stream[Vector[List[Boolean]]] =
    Vector(List.empty) #:: genBinary.map(bs => bs.map(false :: _) ++ bs.map(true :: _))
}
