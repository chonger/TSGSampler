package enbuske.programs

import parse._
import tsg._

/**
 * This prepares sampling data for c++ by
 *
 * serializing the PCFG rules and precalculating their probabilities
 * representing the trees 
 *
 */ 

object AirPack {
  def main(args : Array[String]) : Unit = {
    val ap = new Airplane(args(0))
    ap.packHDP()
    1
  }
}


object AirUnPack {
  def main(args : Array[String]) : Unit = {
    val ap = new Airplane(args(0))
    ap.writeHDP()
    1
  }
}
