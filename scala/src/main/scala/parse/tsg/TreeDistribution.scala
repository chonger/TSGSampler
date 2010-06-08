package parse.tsg

import train.SegmentRef
import train.Markers

abstract class TreeDistribution {
  def score(tree : SegmentRef) : Double
  
  def score(tree : ParseTree) : Double = {
    val mTree = new ParseTree(TreeNode.addParentPtr(tree.root)) with Markers
    val sref = new SegmentRef(mTree.root,mTree,mTree.root,true)
    score(sref)
  }

}

class PCFGDistribution(val pcfg: PCFG) extends TreeDistribution {
  def score(tree : SegmentRef) : Double = {
	tree.pcfgScore(pcfg)
  }
}


class CohnGoldwater(val pcfg : PCFG, val betas : Array[Double]) extends TreeDistribution {


  def score(tree : SegmentRef) : Double = {
    val score = tree.pcfgScore(pcfg)
	val ret = (score /: tree.nonterminals)((a,b) => b match {
	  case un : UnderspecifiedNode => a * (1 - betas(b.symbol))
	  case _ => a * betas(b.symbol)
	})
    ret
  }
}
