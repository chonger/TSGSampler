package enbuske.programs

import parse._
import tsg._
import java.io.{BufferedWriter,FileWriter,File}
import util.Trinarizer
import util.PTClassifier

object FormFunctionTag {
  def main(args : Array[String]) : Unit = {
    val pcfg = new DirectlyEstimatedPCFG()
    val raw = pcfg.read(args(0))
    val (syms,terms) = pcfg.size

    println("PCFG created with " + syms + " symbols and " + terms + " terminals")
    
    val data = raw.map(r => new ParseTree(r.root) with Markers)

    val toTag = pcfg.read(args(2))

    pcfg.process(data ::: toTag)
    //pcfg.setLock(true)

    val tDist = new PCFGDistribution(pcfg)
    
    val packer = new TSGPackager(pcfg)
    val ptsg = packer.unpack(data.toArray,args(1))

    ptsg.smoothFac = args(4).toDouble

    ptsg.addPCFGRules(toTag)

    //ptsg.addAllTags()

    var ind = 0

    val tagged = toTag.map(tree => {

      ind = ind + 1
      println(ind + " - " + tree.nonterminals.length)
      
      val taggings = ptsg.tagNodes(tree)
      
      def tag(n : NonTerminalNode) : NonTerminalNode = {
        
        val realTag = taggings.getOrElse(new RefWrapper(n),n.symbol)
        
	    n match {
	   	  case PreTerminalNode(s,k) => {
            PreTerminalNode(realTag,TerminalNode(k.terminal))
          }
	      case pn : ProtoNode => ProtoNode(realTag,pn.children.map(tag(_)))
	    }
      }

      val newTree = new ParseTree(tag(tree.root))      
      /**
      println(PCFGPrinter.treeToString(pcfg,tree))
      taggings.foreach(_ match {
        case (key,valu) => {
          println(PCFGPrinter.nodeString(pcfg,key.n) + " ===> " + pcfg.symbolStrings(valu))
          
        }
      })
      println(PCFGPrinter.treeToString(pcfg,newTree))
      */

      newTree
    })
    
    val ptc = new PTClassifier(pcfg)

    val ret = ptc.revert(Trinarizer.revert(tagged,pcfg),pcfg)

    println(args(3))
    pcfg.write(args(3),ret)

    1
  }
}
