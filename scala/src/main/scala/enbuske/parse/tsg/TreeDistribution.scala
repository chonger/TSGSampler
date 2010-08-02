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

    var hasTag = false

	val ret = (score /: tree.nonterminals)((a,b) => {

      val str = pcfg.symbolStrings(b.symbol)
      if(str.indexOf("-") > 0)
        hasTag = true
      
      b match {
	    case un : UnderspecifiedNode => {
          
          //if it cuts a head chain then penalize
          
          a * (betas(b.symbol))
        }
	    case _ => a * (1 - betas(b.symbol))
	  }
    })

    hasTag = true
    if(hasTag)
      ret
    else
      ret * Math.pow(10,-20)
  }
}
