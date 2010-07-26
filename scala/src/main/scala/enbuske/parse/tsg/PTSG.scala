package enbuske.parse.tsg

import scala.collection.mutable.{HashMap,HashSet}
import scala.collection.mutable.Stack

class PTSG(val pcfg : PCFG, var counts : HashMap[ParseTree,Int], 
           val dist : TreeDistribution, val alphas : Array[Double]) {

  var scoreMap = new HashMap[ParseTree,Double]()
  var pcfgSet = new HashSet[ParseTree]()
  var headMap : Array[List[ParseTree]] = null
  var headTotals : Array[Double] = null
  
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
    /**
    println("Started with " + counts.size + " trees")
    var added = 0
    pcfgSet.foreach(pt => {
      if(!counts.isDefinedAt(pt)) {
        added += 1
        counts += (pt -> 0)
      }
    })
    println("ADDED " + added + " PCFG RULES")
    */
    fillCaches()
  }

  //add in all of the pcfg rules with zero count
  addPCFGRules(counts.map(_ match {
    case (tree,count) => tree}).toList)    

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
        scoreMap += tree -> scoreDP(tree)
      }
    })
  }


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

  //find the derivations of a given Parse Tree using this PTSG
  def getDerivations(tree : ParseTree) : List[SegTree] = {
    println(PCFGPrinter.treeToString(pcfg,tree))

    getDerivations(tree.root,tree.root.symbol)
  }

  def getDerivations(node : NonTerminalNode, sym : ParseTypes.Symbol) : List[SegTree] = {
    //get all the segments in the grammar which could start from this node
    val validSegs = headMap(sym)

    //for each segment, overlay it from this node
    //if sucessful, we get the nonterminal leaves 
    var derivs = validSegs.map(tree => {

      val kids : List[Tuple2[NonTerminalNode,ParseTypes.Symbol]] = walkWith(node,tree.root)

      if(kids == null) { //match failed, this adds no derivations
        Nil 
      } else if(kids.size == 0) {
        List(new SegTree(tree,Nil))
      } else {
        //println("MATCHED SEGMENT \n" + PCFGPrinter.treeToString(pcfg,tree))
        //println("Got " + kids.size + " kids")
        //if kids is Nil, then this segment completely derives the tree from node
        //and this will still return nil

        val nextDeriv : List[List[SegTree]] = kids.map(x => getDerivations(x._1,x._2))



        if(nextDeriv.filter(_.size == 0).size > 0) { //
          //println("Kid Deriv failed")
          Nil 
        } else {
          //val lst = List(List("a1","a2","a3"),List("b1","b2","b3"),List("c1","c2","c3"))
          
          //get all combinations
          val allCombs : List[List[SegTree]] = getPerm(nextDeriv)
          
          allCombs.map(a => new SegTree(tree,a))
          
        } 


      }
    })

    derivs.flatMap(a => a);
  }

  def getPerm[A](lst : List[List[A]]) : List[List[A]] = {
    if(lst.size == 1)
      return lst(0).map(List(_))
    
    val tails = getPerm(lst.drop(1))
    
    lst(0).map(head => {
      tails.map(head :: _)
    }).flatMap(a => a)
  } 

  /**
   * concurrently walks two trees, accumulating the leaves in target tree which
   * match the segment in segNode.  If there is not a match, then return is null,
   * but if the segment completely covers the target tree the return value is Nil
   */ 
  def walkWith(target : NonTerminalNode, segNode : NonTerminalNode) 
    : List[Tuple2[NonTerminalNode,ParseTypes.Symbol]] = {

    val tStr = pcfg.symbolStrings(target.symbol)
    val sStr = pcfg.symbolStrings(segNode.symbol)

    if(sStr contains '-') {
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

  

  def getTagged(sym : String) : List[ParseTypes.Symbol] = {
    pcfg.symbolStrings.filter(_.indexOf(sym) == 0).map(s => pcfg.symbolIDs(s))
  }


  def tagNodes(tree : ParseTree) : HashMap[RefWrapper,ParseTypes.Symbol] = {

    /**
     * for each (node, tag), get all the segments that could be
     * overlayed on the tree from that node
     */
    var nodeToSeg = new HashMap[Tuple2[RefWrapper,ParseTypes.Symbol],List[ParseTree]]()

    var tCan = 0

    tree.nonterminals.foreach(n => {
      val rw = new RefWrapper(n)
      
      //get the tagged symbols which could come from here
      val tags = getTagged(pcfg.symbolStrings(n.symbol))
      
      tags.map(t => {

        //get all trees which might start with this tag
        val cand = headMap(t).filter(seg => {
          walkWith(n,seg.root) != null
        })
        tCan += cand.length
        nodeToSeg += (rw,t) -> cand
        /**
        if(cand.length == 0)
          println("NO CANDS FOR " + PCFGPrinter.nodeString(pcfg,n))
        else {
          println("SEGS FROM " + PCFGPrinter.nodeString(pcfg,n) + " IN ")
          println(PCFGPrinter.treeToString(pcfg,tree))
          cand.foreach(c => {
            println(PCFGPrinter.treeToString(pcfg,c))
            
          })
        }

        println("DONE")
        */
      })
    })

    println("TOTAL CANDIDATES = " + tCan)

    
    /**
     * base cases -
     * outside probability of the root
     * inside probability of the preterminals
     *
     * inside - for a node N , we
     *    assume that all derived internal nodes have had their inside probabilities
     *    calculated.  for each rule R which rewrites N, the inside is that rule's
     *    probability times the inside probability of each of its leaves
     *    the total inside is the sum over all R
     *
     * outside - we assume all ancestor nodes have their outside calculated
     *    we assume that all nodes have their inside probabilities calculated.
     *    we need a back reference for all rule expansions which have this node in their
     *    leaf list. we will need the index in the leaf list stored as well, b/c the outside
     *    for a rule is that rules probability times the outside prob of its root and
     *    the inside probs of all of its leaves.  The total is a sum over the rules which have
     *    this is a leaf.
     *
     * 
     * using them - for a rule, once we know the inside of its leaves L
     *    and the outside of its root R, it contributes its probability
     *    times all of these items to the score for each tag in its tree.
     *    
     */ 
    
    var insideMap = new HashMap[Tuple2[RefWrapper,ParseTypes.Symbol],Double]()

    def inside(n : NonTerminalNode, sym : ParseTypes.Symbol) : Double = {

      val rw = new RefWrapper(n)

      insideMap.get((rw,sym)) match {
        case Some(v) => return v
        case None => {
          //do nothing
        }
      }
      
            
      val segs = nodeToSeg(rw,sym) //the (tag,List[segments]) that can overlay from this node
      
      //sometimes, this is empty...this makes all inside probs above it zero...

      var iProb : Double = 0.0
      segs.foreach(e => {        
        val leaves = walkWith(n,e.root) 
          
        if(leaves == null)
          throw new Exception()
          
        if(scoreMap(e) == 0)
          throw new Exception()

        val sProb = (scoreMap(e) /: leaves)(
          (a,l) => a * inside(l._1,l._2) //the recursive call
        )
        
        iProb = iProb + sProb
      })

      //if(iProb == 0)
      //  throw new Exception()

      insideMap += (rw,sym) -> iProb
      iProb
    }
    
    //printTree(tree)

    inside(tree.root,tree.root.symbol) //assumes that root is never split

    //println("INSIDE DONE");




    var outsideMap = new HashMap[Tuple2[RefWrapper,ParseTypes.Symbol],Double]()

    outsideMap += (new RefWrapper(tree.root),tree.root.symbol) -> 1.0

    def calcOutside(n : NonTerminalNode) : Unit = {

      //println("CALC OUTSIDE")
      //printNode(n)

      val tags = getTagged(pcfg.symbolStrings(n.symbol))
      tags.foreach(sym => {
        if(outsideMap.isDefinedAt((new RefWrapper(n),sym))) {

          val rw = new RefWrapper(n)
          val segs = nodeToSeg(rw,sym) //the (tag,List[segments]) that can overlay from this node
          
          var oProb : Double = 0.0
          segs.foreach(e => {        
            val leaves = walkWith(n,e.root) 
            if(leaves == null)
              throw new Exception()
            
            //the outside prob at the leaf is prob(e) * outside at n * inside of other leaves
            var oProb = outsideMap(rw,sym) * scoreMap(e) 
            
            //val lIns = leaves.map(l => inside(l._1,l._2))
            
            leaves.foreach(l => {
              val myProb = (oProb /: leaves)((a,al) => {
                if(l == al)
                  a
                else {
                  a * insideMap(new RefWrapper(al._1),al._2)
                }
              })
              
              val key = (new RefWrapper(l._1),l._2)
                val tot = outsideMap.getOrElse(key,0.0)
              outsideMap += key -> (tot + myProb)
            })
          })          
        }
      })

      n match {
        case in : InternalNode => {
          in.children.foreach(c => {
            calcOutside(c)
          })
        }
        case _ => {}
      }
          

    }

    calcOutside(tree.root) //assumes that root is never split

    //println("OUTSIDE DONE")
/**
    println("OUTSIDE ELEMENTS")
    outsideMap.foreach(_ match {
      case ((n,t),v) => {
        printNode(n.n)
        println(pcfg.symbolStrings(t) + " --- " + v)
      }
    })
    println("INSIDE ELEMENTS")
    insideMap.foreach(_ match {
      case ((n,t),v) => {
        printNode(n.n)
        println(pcfg.symbolStrings(t) + " --- " + v)
      }
    })
*/
    /**
     *  Now to use the segments
     *
     *
     *
     */

    val scores = new HashMap[RefWrapper,List[Tuple2[ParseTypes.Symbol,Double]]]()

    def scoreTag(target : NonTerminalNode, segNode : NonTerminalNode, score : Double) : Unit = {

      val rw = new RefWrapper(target)
      var cur = scores.getOrElse(rw,Nil)

      var found = false
      cur = cur.map(_ match {
        case (sym,scr) => {
          if(sym == segNode.symbol) {
            found = true
            (sym,scr + score)
          } else
            (sym,scr)
        }
      })
      if(!found) {
        cur ::= (segNode.symbol,score)
      }
      
      scores += rw -> cur


      //recurse
      segNode match {
        case un : UnderspecifiedNode => {
          //do nothing
        }
        case in : InternalNode => {
          target match {
          case in2 : InternalNode => {
            val ch1 = in.children
            val ch2 = in2.children
            val chZip = ch2 zip ch1
            chZip.foreach(_ match {
              case (t,s) => scoreTag(t,s,score)
            })
          }
          }
          
        }
        case _ => {}
      }	
    }

    //TODO : does this  double count the leaves?
    def recScore(n : NonTerminalNode) : Unit = {
      val rw = new RefWrapper(n)
      
      val tags = getTagged(pcfg.symbolStrings(n.symbol))
      tags.foreach(sym => {
        if(outsideMap.isDefinedAt((new RefWrapper(n),sym))) {
          val segs = nodeToSeg(rw,sym) //the (tag,List[segments]) that can overlay from this node

          segs.foreach(e => {        

            val leaves = walkWith(n,e.root) 
            var scr = scoreMap(e) * outsideMap(rw,sym)
            scr = (scr /: leaves)((a,l) => {
              a * insideMap(new RefWrapper(l._1),l._2)
            })
            /**
             println("SCORE " + scr)
             printNode(n)
             printTree(e)
             */
            scoreTag(n,e.root,scr)
          })
        }
      })

      n match {
        case in : InternalNode => {
          in.children.foreach(c => {
            recScore(c)
          })
        }
        case _ => {}
      }

    }
    recScore(tree.root)

    //println("SCORING DONE")

    //println("Num nonterms = " + tree.nonterminals.length)    

    val ret = new HashMap[RefWrapper,ParseTypes.Symbol]()    
    scores.keySet.foreach(rw => {
      val tags = scores(rw)
      
      if(tags.length == 0)
        throw new Exception()

      //printNode(rw.n)

      var maxV = 0.0
      var best : ParseTypes.Symbol = 0

      tags.foreach(_ match {
        case (t,d) => {
          
          //println(d + " - " + pcfg.symbolStrings(t) + " == " + pcfg.symbolStrings(best))
          
          if(d > maxV) {
            best = t
            maxV = d
          }
        }
      })
      
      ret += rw -> best

    })

    ret
  }

  def printNode(n : NonTerminalNode) = {
    println(PCFGPrinter.nodeString(pcfg,n))
  }

  def printTree(tree : ParseTree) = {
    println(PCFGPrinter.treeToString(pcfg,tree))
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
