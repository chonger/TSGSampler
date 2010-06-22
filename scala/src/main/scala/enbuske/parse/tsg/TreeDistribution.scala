package enbuske.parse.tsg

abstract class TreeDistribution {
  
  def score(tree : ParseTree) : Double

}

class PCFGDistribution(val pcfg: PCFG) extends TreeDistribution {
  def score(tree : ParseTree) : Double = pcfg.scoreTree(tree)
}


class CohnGoldwater(val pcfg : PCFG, val betas : Array[Double]) extends TreeDistribution {
  def score(tree : ParseTree) : Double = {
    val score = pcfg.scoreTree(tree)
	val ret = (score /: tree.nonterminals)((a,b) => b match {
	  case un : UnderspecifiedNode => a * (1 - betas(b.symbol))
	  case _ => a * betas(b.symbol)
	})
    ret
  }
}
