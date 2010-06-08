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
    pcfg.setLock(true)
    val data = raw.map(r => new ParseTree(TreeNode.addParentPtr(r.root)) with FullSpec with Markers)

    val toTag = corpora.treebank.TreebankData.read(args(2),pcfg)

    val tDist = new PCFGDistribution(pcfg)
    
    val packer = new TSGPackager()
    val ptsg = packer.unpack(pcfg,data.toArray,args(1))

    ptsg.addPCFGRules(toTag)

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

    println(args(3))
    corpora.treebank.TreebankData.write(args(3),tagged,pcfg)

    1
  }
}
