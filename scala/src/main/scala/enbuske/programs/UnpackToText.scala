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
    pcfg.process(raw)

    val data = raw.map(r => new ParseTree(r.root) with Markers)
    
    val packer = new TSGPackager(pcfg)
    val ptsg = packer.unpack(data.toArray,args(1))
    
    val bw = new BufferedWriter(new FileWriter(new File(args(2))))
    ptsg.counts.elements.toList.sort((a,b) => {a._2 > b._2}).foreach(_ match {
      case (a,b) => {
        //if(pcfg.getSym(a.root).indexOf("NP") == 0) {
          bw.write(PCFGPrinter.treeToString(pcfg,a) + "\n")
          bw.write(b + "\n")
        //}
      }
    })
    bw.close()

    1
  }

}

class UnpackerToText(rawF : String, packedF : String, start : Int, end : Int) {
  val pcfg = new DirectlyEstimatedPCFG()
  val raw = pcfg.read(rawF)
  pcfg.process(raw)
  
  val data = raw.map(r => new ParseTree(r.root) with Markers)
  
  val packer = new TSGPackager(pcfg)
  val ptsg = packer.unpack(data.toArray,packedF)

  def options() = {
    pcfg.symbolStrings.foreach(a => println(a))
  }

  def apply(s : String) = {
    val segs = ptsg.headMap(pcfg.symbolIDs(s))
   
    segs.sort((a,b) => {ptsg.scoreMap(a) > ptsg.scoreMap(b)}).slice(0,10).foreach(e => {
      println(PCFGPrinter.treeToString(pcfg,e))
      println(ptsg.scoreMap(e))
    })

    println("Z = " + (0.0 /: segs)((a,b) => a + ptsg.scoreMap(b)))

  }

  def writeAll(filename : String) = {
    val bw = new BufferedWriter(new FileWriter(new File(filename)))
    pcfg.symbolStrings.foreach(s => {
      val segs = ptsg.headMap(pcfg.symbolIDs(s))
   
      segs.sort((a,b) => {ptsg.scoreMap(a) > ptsg.scoreMap(b)}).slice(0,20).foreach(e => {
        bw.write(PCFGPrinter.treeToString(pcfg,e) + "\n")
        bw.write(ptsg.scoreMap(e) + "\n")
      })

      bw.write("Z = " + (0.0 /: segs)((a,b) => a + ptsg.scoreMap(b)) + "\n------------------------------------------------\n")
    })
    bw.close()
  }

}
