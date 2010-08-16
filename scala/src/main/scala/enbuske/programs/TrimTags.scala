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

object TrimTags {
  def main(args : Array[String]) : Unit = {
    val pcfg = new DirectlyEstimatedPCFG()
    val raw = pcfg.read(args(0))
    pcfg.process(raw)
    val data = raw.map(r => new ParseTree(r.root) with Markers)
    val packer = new TSGPackager(pcfg)
    val trimData = packer.trimTagged(data)
    println("Trimmed out " + (data.length - trimData.length))
    pcfg.write(args(1),trimData)
    1
  }
}
