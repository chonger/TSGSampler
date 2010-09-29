package enbuske.parse.tsg

import scala.collection.mutable.{HashMap,HashSet}
import scala.collection.mutable.Stack

class ETreeDP(val pcfg : PCFG, var counts : HashMap[ParseTree,Int], 
              val dist : TreeDistribution, val alphas : Array[Double]) {

  var scoreMap = new HashMap[ParseTree,Double]()
  var pcfgSet = new HashSet[ParseTree]()
  var headMap : Array[List[ParseTree]] = null
  var headTotals : Array[Double] = null

  //add in extra pcfg rules, this also fills the caches
  addPCFGRules(counts.map(_ match {
    case (tree,count) => tree}).toList)    

  
  /**
  println("Sizes!")
  headMap.zipWithIndex.foreach(_ match {
    case (x,c) => println(pcfg.symbolStrings(c) + " -> " + (x.length))
  })
  println("Alphas!")
  alphas.zipWithIndex.foreach(_ match {
    case (x,c) => println(pcfg.symbolStrings(c) + " -> " + (x))
  })
  println("Betas!")
  val bs = dist.asInstanceOf[CohnGoldwater].betas
  bs.zipWithIndex.foreach(_ match {
    case (x,c) => println(pcfg.symbolStrings(c) + " -> " + (x))
  })
  */

  
  //adds in all possible the PCFG rules with zero counts in data
  def addPCFGRules(data : List[ParseTree]) = {
    
    data.foreach(tree => {
      val nnodes = tree.nonterminals.filter(!_.isInstanceOf[UnderspecifiedNode])
      nnodes.foreach(n => {
        n match {
          case pn : PreTerminalNode => {
            pcfgSet += new ParseTree(pn)
          }
          case in : InternalNode => {
            val newkids = in.children.map(n2 => UnderspecifiedNode(n2.symbol,null))
            pcfgSet += new ParseTree(ProtoNode(in.symbol,newkids))
          }
        }
      })
    })
    
    println("Started with " + counts.size + " trees")
    var added = 0
    pcfgSet.foreach(pt => {
      if(!counts.isDefinedAt(pt)) {
        println(PCFGPrinter.treeToString(pcfg,pt))
        added += 1
        counts += (pt -> 0)
      }
    })
    println("ADDED " + added + " PCFG RULES")
    
    fillCaches()
  }


  def fillCaches() = {
    /**
     * build a map of symbol ids to the segments they are root of
     * also get the total count of segments which extend this symbol
     */ 

    headMap = (for{i <- 0 to pcfg.nextSymID} yield {
      Nil
    }).toArray
    counts.foreach(_ match {
      case (tree,count) => {
        headMap(tree.root.symbol) ::= tree
      }
    })

    headTotals = headMap.map(lst => {
      (0.0 /: lst)((a,b) => a + counts(b))
    })

    /**
     *  cache the scores for each segment as determined by the DP
     */
    scoreMap.clear()
    counts.foreach(_ match {
      case (tree,count) => {
        scoreMap += tree -> score(tree)
      }
    })
  }

  def printNode(n : NonTerminalNode) = {
    println(PCFGPrinter.nodeString(pcfg,n))
  }

  def printTree(tree : ParseTree) = {
    println(PCFGPrinter.treeToString(pcfg,tree))
  }

  def score(segTree : SegTree) : Double = {
    (score(segTree.segment) /: segTree.children)((a,b) => a * score(b))
  }


  def score(tree : ParseTree) : Double = {
    val sym = tree.root.symbol
    (alphas(sym) * dist.score(tree) + counts.getOrElse(tree,0)) /
    (alphas(sym) + headTotals(sym))
  }

  def sample(s : ParseTypes.Symbol) : ParseTree = {

    val segs = headMap(s)

    var total = 0.0
    val scored = segs.map(seg => {
      val scr = scoreMap(seg)
      total += scr
      (seg,scr)
    })

    val rando = util.Util.javaRandom.nextDouble * total

    var runTot = 0.0
    scored.foreach(p => {
      runTot += p._2
      if(rando < runTot)
        return p._1
    })
    
    throw new Exception("!")
  }

}

