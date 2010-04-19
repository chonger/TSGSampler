package parse.test

import parse.tsg._
import train._

object ThreadTSG {
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

    val maxNodes = (0 /: data)((max,tree) => {
      if(tree.nodes.length > max)
        tree.nodes.length
      else
        max
    }) 
    println("Maximum tree size = " + maxNodes)
    println("NTRULES = " + pcfg.rules.size)
    println("TMRULES = " + pcfg.lexiconRules.size)
    println("Total number of rules = " + (pcfg.rules.size + pcfg.lexiconRules.size))

    //data.foreach(_.randomizeMarkers)
    /**
    data.foreach(t => {
    	println(PCFGPrinter.treeToString(pcfg,t))
    	t.getSegments.foreach(s => println(PCFGPrinter.treeToString(pcfg,s)))
    })
    */
    
    println("Set")
    val tDist = new PCFGDistribution(pcfg)
    val numTuDi = args(2).toInt
    println("Creating TSG ShiFu with " + numTuDi + " TuDi")
    val tsgShifu = new TSGShiFu(new TSGTrainer(tDist,data.toArray) with ConcentrationParameterResampling,numTuDi,scala.actors.Actor.self)
    println("Go")
    val startSmooth = args(3).toDouble
    val numMixIter = args(4).toInt
    println("Training TuDi from " + startSmooth + " to 1.0 for " + numMixIter + " iterations")
    tsgShifu.train(startSmooth,1.0,numMixIter)  	
    scala.actors.Actor.self.receive {
      case result : SampleResult => { //it is important that this result is by itself
    		println("First Stage Complete")
      }
      case err => throw new Exception("Unhandled top level message - " + err)
    }
    
    val numCongealIter = args(5).toInt
    println("Training TuDi from 1.0 to 1.0 for " + numCongealIter + " iterations")
    tsgShifu.train(1.0,1.0,numCongealIter)
    val segs : List[ParseTree] = scala.actors.Actor.self.receive {
    	case result : SampleResult => { //it is important that this result is by itself
    		result.counts.elements.map(_._1).toList
    	}
    	case err => throw new Exception("Unhandled top level message - " + err)
    }
    
    tsgShifu.trainer.printCounts()
    println("Writing Segments to " + args(1))
    corpora.treebank.TreebankData.write(args(1),segs,pcfg)
    1
  }
}
