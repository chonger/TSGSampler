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

object AuthorPack {
  def main(args : Array[String]) : Unit = {
    val aid = new AuthorID(args(0))
    aid.packHDP()
    1
  }
}


object AuthorUnPack {
  def main(args : Array[String]) : Unit = {
    val aid = new AuthorID(args(0))
    aid.classifyAllHDP()
    1
  }
}
