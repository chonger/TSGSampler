package enbuske.programs

import java.io.{BufferedWriter,FileWriter,File}
import parse._
import tsg._

/**
 * Once you've sampled a PTSG, why not look at it?
 *
 * This program reads trees from arg0, c++ sampling data from arg1
 * and writes the elementary trees and their counts to arg2
 *
 * then you can look at them in a text editor
 * 
 */ 

object UnpackToText {
  def main(args : Array[String]) : Unit = {
    val pcfg = new DirectlyEstimatedPCFG()
    val raw = pcfg.read(args(0))
    val (syms,terms) = pcfg.size
    println("PCFG created with " + syms + " symbols and " + terms + " terminals")

    println("Estimating probabilities")
    pcfg.process(raw)

    val data = raw.map(r => new ParseTree(r.root) with Markers)
    
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
