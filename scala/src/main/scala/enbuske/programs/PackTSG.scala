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
    val (syms,terms) = pcfg.size
    println("PCFG created with " + syms + " symbols and " + terms + " terminals")
    print("Estimating PCFG ... ")
    pcfg.process(raw)
    println("done")
    val data = raw.map(r => new ParseTree(r.root) with Markers)

    
    val packer = new TSGPackager()
    packer.packageTrainer(pcfg,data,args(1))
    
    1
  }
}
