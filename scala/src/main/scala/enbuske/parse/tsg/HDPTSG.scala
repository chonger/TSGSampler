package enbuske.parse.tsg

import scala.collection.mutable.{HashMap,HashSet}
import scala.collection.mutable.Stack

class HDPTSG(val pcfg : PCFG, 
             var counts : HashMap[ParseTree,Int], 
             var baseAlpha : Array[Double],
             var mixCounts : Array[HashMap[ParseTree,Int]],
             var mixAlpha : Array[Array[Double]],
             val mixWeights : Array[Array[Array[Double]]],
             val dist : TreeDistribution) {

  val numDP = mixCounts.length

  var scoreMap : Array[HashMap[ParseTree,Double]] = null
  var pcfgSet = new HashSet[ParseTree]()
  var headMap : Array[List[ParseTree]] = null
  var headZ : Array[Array[Double]] = null
  var headTotals : Array[Array[Double]] = null
  var baseTotals : Array[Double] = null


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
    
      //println(PCFGPrinter.treeToString(pcfg,pt))
      if(!counts.isDefinedAt(pt)) {
        added += 1
        counts += (pt -> 0)
      }


    })
      
    println("ADDED " + added + " PCFG RULES")

    //this effectively adds them to the base map
    fillCaches()
  }

  def fillCaches() = {
    /**
     * build a map of symbol ids to the segments they are root of
     * also get the total count of segments which extend this symbol
     */ 

    //headMap is a list for each LHS of the trees with that root
    //since the base has every tree in a mixDP, this will cover all possible Segments
    headMap = (for{i <- 0 to pcfg.nextSymID} yield {
      Nil
    }).toArray
    counts.foreach(_ match {
      case (tree,count) => {
        headMap(tree.root.symbol) ::= tree
      }
    })

    //headTotals is a list of the total counts of trees 
    headTotals = (for{i <- 0 to numDP - 1} yield {
      //for each DP, get the counts for each LHS
      headMap.map(lst => {
        (0.0 /: lst)((a,b) => {
          if(mixCounts(i).isDefinedAt(b))
            a + (mixCounts(i))(b)
          else
            a
        })
      })
    }).toArray

    baseTotals = headMap.map(lst => {
      (0.0 /: lst)((a,b) => {
          a + counts(b)
      })
    }).toArray

    headZ = (for{i <- 0 to numDP - 1} yield {
      //for each DP, sum the scores
      headMap.map(lst => {
        (0.0 /: lst)((a,b) => {
          a + scoreDP(b,i)
        })
      })
    }).toArray

    /**
     *  cache the scores for each segment as determined by the DP
     */
    scoreMap = (for{i <- 0 to numDP - 1} yield {
      var ret = new HashMap[ParseTree,Double]()
      counts.foreach(_ match {
        case (tree,count) => {
          val Z = headZ(i)(tree.root.symbol)
          ret += tree -> (scoreDP(tree,i) / Z)
        }
      })
      ret
    }).toArray
  }

  def scoreTree(tree : ParseTree, aspect : Int) : Double = {
    
    var nodeToSeg = new HashMap[RefWrapper,List[ParseTree]]()

    tree.nonterminals.foreach(n => {
      val rw = new RefWrapper(n)
      var myCan = 0

      //get all trees which might start with this tag
      val cand = headMap(n.symbol).filter(seg => {
          walkWith(n,seg.root) != null
      })
      
      myCan += cand.length
      nodeToSeg += rw -> cand

      if(myCan == 0) {
        println("NO SEGS FROM " + PCFGPrinter.nodeString(pcfg,n))
        println("There should be some segment from here")
        throw new Exception();
      }
    })
 
    var insideMap = new HashMap[RefWrapper,Double]()

    def doinside(n : NonTerminalNode) : Unit = {
      val rw = new RefWrapper(n)

      n match {
        case pn : PreTerminalNode => {
          //do nothing
        }
        case in : InternalNode => {
          in.children.foreach(a => doinside(a))
        }
      }

    
      val segs = nodeToSeg(rw)
      var iProb : Double = 0.0
      segs.foreach(e => {        
        val leaves = walkWith(n,e.root) 
          
        if(leaves == null)
          throw new Exception()
          
        if(scoreMap(aspect)(e) == 0)
          throw new Exception()

        val sProb = (scoreMap(aspect)(e) /: leaves)(
          (a,l) => a * insideMap(new RefWrapper(l._1)) //the recursive call
        )
        
        iProb = iProb + sProb
      })

      
        
      val cur = insideMap.getOrElse(rw,0.0)

      insideMap += rw -> (cur + iProb)
    }

    doinside(tree.root) //assumes that root is never split

    val ret = insideMap(new RefWrapper(tree.root))
/**
    if(ret == 0) {
      insideMap.foreach(e => {
        println(PCFGPrinter.nodeString(pcfg,e._1.n))
        println(e._2)
      })
      throw new Exception
    }
*/
    ret
  }


  def walkWith(target : NonTerminalNode, segNode : NonTerminalNode) 
    : List[Tuple2[NonTerminalNode,ParseTypes.Symbol]] = {

    val tStr = pcfg.symbolStrings(target.symbol)
    val sStr = pcfg.symbolStrings(segNode.symbol)

    
    if((sStr contains '-') && (sStr.indexOf('-') > 0)) {
      val subS = sStr.substring(0,sStr.indexOf('-'))

      if(!subS.equals(tStr))
         return null

    } else {
      if(target.symbol != segNode.symbol)
        return null
    }


    segNode match {
      case un : UnderspecifiedNode => {
        List((target,segNode.symbol))
      }
      case pn : PreTerminalNode => {
        target match {
          case pn2 : PreTerminalNode => {
            if(pn.kid.terminal == pn2.kid.terminal)
              return Nil
            else
              return null
          }
          case _ => return null
        }
      }
      case in : InternalNode => {
        target match {
          case in2 : InternalNode => {
            val ch1 = in.children
            val ch2 = in2.children
          
            if(ch1.size != ch2.size) {
              return null
            }

            val chZip = ch2 zip ch1

            val walkZ = chZip.map(x => x match {
              case (a : NonTerminalNode,b : NonTerminalNode) => walkWith(a,b)
              case _ => println(x);throw new Exception
            })



            if(walkZ.filter(_ == null).size > 0)
              return null



            return walkZ.flatMap(a => a)
          }
          case _ => return null
        }

      }
    }	
  }

  

  def scoreDP(tree : ParseTree, aspect : Int) : Double = {
    val sym = tree.root.symbol

    val baseScore = (baseAlpha(sym) * dist.score(tree) + counts.getOrElse(tree,0)) /
    (baseAlpha(sym) + baseTotals(sym))

    (mixAlpha(aspect)(sym) * baseScore + mixCounts(aspect).getOrElse(tree,0)) /
    (mixAlpha(aspect)(sym) + headTotals(aspect)(sym))

  }


}
