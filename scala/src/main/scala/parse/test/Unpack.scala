package parse.test

import parse.tsg._
import train._
import java.io.{BufferedWriter,FileWriter,File}

object Unpack {
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
    

    
    val bw = new BufferedWriter(new FileWriter(new File(args(2))))
    ptsg.counts.elements.toList.sort((a,b) => {a._2 > b._2}).foreach(_ match {
      case (a,b) => {
        bw.write(PCFGPrinter.treeToString(pcfg,a) + "\n")
        bw.write(b + "\n")
      }
    })
    bw.close()

    1
  }
}
