package parse.tsg

import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

class PTSG(val pcfg : PCFG, val counts : HashMap[ParseTree,Int], 
           val dist : TreeDistribution, val alphas : Array[Double]) {


  var headMap : Array[List[ParseTree]] = (for{i <- 1 to pcfg.nextSymID} yield {
    Nil
  }).toArray
  counts.foreach(_ match {
    case (tree,count) => headMap(tree.root.symbol) ::= tree
  })

  var headTotals : Array[Int] = headMap.map(_.length)

  println("Sizes!")
  headMap.zipWithIndex.foreach(_ match {
    case (x,c) => println(pcfg.symbolStrings(c) + " -> " + (x.length))
  })

  //find the derivations of a given Parse Tree using this PTSG
  def getDerivations(tree : ParseTree) : List[SegTree] = getDerivations(tree.root)

  def getDerivations(node : NonTerminalNode) : List[SegTree] = {
    //get all the segments in the grammar which could start from this node
    val validSegs = headMap(node.symbol)

    //for each segment, overlay it from this node
    //if sucessful, we get the nonterminal leaves 
    validSegs.map(tree => {
      val kids : List[NonTerminalNode] = walkWith(node,tree.root)
/**
      if(kids != null) //match failed, this adds no derivations
        Nil
      else {
        //if kids is Nil, then this segment completely derives the tree from node
        kids.map(getDerivations(_))
      }
*/
      null

    })

    
  }

    
  def walkWith(target : NonTerminalNode, segNode : NonTerminalNode) : List[NonTerminalNode] = {
    null
  }

  def score(segTree : SegTree) : Double = {
    (scoreDP(segTree.segment) /: segTree.children)((a,b) => a * score(b))
  }


  def scoreDP(tree : ParseTree) : Double = {
    val sym = tree.root.symbol
    (alphas(sym) * dist.score(tree) + counts.getOrElse(tree,0)) /
    (alphas(sym) + headTotals(sym))
  }

}
