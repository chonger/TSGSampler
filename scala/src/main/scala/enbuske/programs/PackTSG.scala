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

object PackTSG {
  def main(args : Array[String]) : Unit = {
    val pcfg = new DirectlyEstimatedPCFG()
    val raw = pcfg.read(args(0))
    pcfg.process(raw)
    val data = raw.map(r => new ParseTree(r.root) with Markers)
    val packer = new TSGPackager(pcfg)
    packer.packageOne(data,args(1))
    1
  }
}
