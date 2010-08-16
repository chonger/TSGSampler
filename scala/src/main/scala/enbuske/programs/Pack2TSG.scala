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

object Pack2TSG {
  def main(args : Array[String]) : Unit = {
    val pcfg = new DirectlyEstimatedPCFG()
    val rawB = pcfg.read(args(0))
    val rawS = pcfg.read(args(2))
    pcfg.process(rawB ::: rawS)
    val dataB = rawB.map(r => new ParseTree(r.root) with Markers)
    val dataS = rawS.map(r => new ParseTree(r.root) with Markers)
    val packer = new TSGPackager(pcfg)
    packer.packageTwo(dataB,args(1),dataS,args(3))
    1
  }
}
