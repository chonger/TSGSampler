package parse.test

import parse.tsg._
import train._

object TrainTSG {
  def main(args : Array[String]) : Unit = {
    val pcfg = new DirectlyEstimatedPCFG()
    val raw = corpora.treebank.TreebankData.read(args(0),pcfg)
    val (syms,terms) = pcfg.size
    println("PCFG created with " + syms + " symbols and " + terms + " terminals")
    print("Estimating PCFG ... ")
    pcfg.process(raw)
    println("done")
    println("Ready")
    val data = raw.map(r => new ParseTree(TreeNode.addParentPtr(r.root)) with FullSpec with Markers)
    //data.foreach(_.randomizeMarkers)
    /**
    data.foreach(t => {
    	println(PCFGPrinter.treeToString(pcfg,t))
    	t.getSegments.foreach(s => println(PCFGPrinter.treeToString(pcfg,s)))
    })
    */
    println("Set")
    val tDist = new PCFGDistribution(pcfg)
    val tsgTrainer = new TSGTrainer(tDist,data.toArray)
    tsgTrainer.initFromData

    val packer = new TSGPackager()
    packer.packageTrainer(pcfg,tsgTrainer,args(1))

    /**
    println("Go")
    tsgTrainer.train(3,1,100)
    tsgTrainer.train(1,1,200)
    tsgTrainer.printCounts()
    val segs = tsgTrainer.readSegments()
    println("Writing Segments to " + args(1))
    corpora.treebank.TreebankData.write(args(1),segs,pcfg)
    */
    1
  }
}
