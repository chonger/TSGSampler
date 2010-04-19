package tools

import parse._
import tsg._
import tsg.train._
import java.io.{BufferedWriter,FileWriter,File}

object FormFunctionTag {
  def main(args : Array[String]) : Unit = {
    val pcfg = new DirectlyEstimatedPCFG()
    val raw = corpora.treebank.TreebankData.read(args(0),pcfg)
    val (syms,terms) = pcfg.size
    println("PCFG created with " + syms + " symbols and " + terms + " terminals")
    pcfg.process(raw)

    val data = raw.map(r => new ParseTree(TreeNode.addParentPtr(r.root)) with FullSpec with Markers)

    val tDist = new PCFGDistribution(pcfg)
    
    val packer = new TSGPackager()
    val ptsg = packer.unpack(pcfg,data.toArray,args(1))

    
    

    1
  }
}
