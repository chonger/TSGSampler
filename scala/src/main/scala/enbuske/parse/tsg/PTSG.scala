package enbuske.parse.tsg

import scala.collection.mutable.{HashMap,HashSet}
import scala.collection.mutable.Stack

class PTSG(val pcfg : PCFG, var counts : HashMap[ParseTree,Int], 
           val dist : TreeDistribution, val alphas : Array[Double]) {

  var smoothFac = 1.0

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

  //get all symbols which start with this string
  def getTagged(sym : String) : List[ParseTypes.Symbol] = {
    pcfg.symbolStrings.filter(_.indexOf(sym) == 0).map(s => pcfg.symbolIDs(s))
  }

  //should be an UNTAGGED form
  def getTags(sym : String) : List[ParseTypes.Symbol] = {
    import scala.collection.mutable.HashSet
    val strs = pcfg.symbolStrings.filter(_.indexOf(sym) == 0)
    val hs = new HashSet[String]()
    strs.foreach(s => hs ++= tagExtract(s))
    hs.elements.map(s => pcfg.symbolIDs(s)).toList
  }

  def removeTag(sym : String) : ParseTypes.Symbol = {
    val dInd = sym.indexOf('-')
    if(dInd <= 0)
      return pcfg.symbolIDs(sym)
    pcfg.symbolIDs(sym.substring(0,dInd))
  }

  def tagExtract(sym : String) : List[String] = {
    val dInd = sym.indexOf('-')
    if(dInd <= 0) //-LRB- et al or none
      return Nil
    sym.substring(dInd + 1).split("-").toList
  }

  val syntactic = List("DTV","LGS","PRD","PUT","SBJ","VOC")
  val semantic =  List("ADV","BNF","DIR","EXT","LOC","MNR","NOM","PRP","TMP")
  val topic = List("TPC")
  val misc = List("CLF","HLN","TTL")
  val related = List("CLR")

  val groups = List(syntactic,semantic,topic,misc,related)
  //when tagging the nodes, allow only one tag from each group.
  //allow a tag from another group if it has probability > tagCutoff
  val tagCutoff : Double = .3
  

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
      
      //var myCan = 0

      tags.map(t => {

        //get all trees which might start with this tag
        val cand = headMap(t).filter(seg => {
          walkWith(n,seg.root) != null
        })
      
        //myCan += cand.length
        
        nodeToSeg += (rw,t) -> cand

      })

      /**
      if(myCan == 0) {
          println("NO SEGS FROM " + PCFGPrinter.nodeString(pcfg,n))
        println("There should be some segment from here")
        throw new Exception();
      }
      */

    })
    
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

    def isClone_?(e : ParseTree, o : ParseTree) : Boolean = {
      if(e.root.children.length != o.root.children.length)
        return false

      e.root.children zip o.root.children foreach (_ match {
        case (en,on) => {
          if(en != on)
            return false
        }
      })

      true
    }

    def doinside(n : NonTerminalNode) : Unit = {
      


      n match {
        case pn : PreTerminalNode => {
          //do nothing
        }
        case in : InternalNode => {
          in.children.foreach(a => doinside(a))
        }
      }
      val rw = new RefWrapper(n)
      val tags = getTagged(pcfg.symbolStrings(n.symbol))

      /**
      //KNESER NEY

      import scala.collection.mutable.HashSet
      var nSet = new HashSet[ParseTree]()

      val rw = new RefWrapper(n)
      val tags = getTagged(pcfg.symbolStrings(n.symbol))
      tags.foreach(tag => nSet ++= nodeToSeg(rw,tag))

      while(!nSet.isEmpty) {
        val e = (nSet.elements.toList)(0)
        nSet -= e
        val clones = nSet.elements.filter(x => isClone_?(e,x))
        nSet -= clones
        val segs = e :: clones
        
        
        // now we smooth this set of segments' probabilities
        // (in general this list will be short) could add a spc case when clones are Nil
        

        val counts = segs.map(s => counts(s))
        
        val n1 = counts.filter(_ == 1).length.toDouble
        val n2 = counts.filter(_ == 2).length.toDouble
        val n3 = counts.filter(_ == 3).length.toDouble
        val n4 = counts.filter(_ == 4).length.toDouble
        lazy val y : Double = if(n1 + n2 == 0) {
          0 .0
        } else 
          n1 /(n1 + 2 * n2)

        lazy val d1 = if(n1 == 0) {
          1.0
        } else 
          1.0 - 2 * y * n2 / n1
        lazy val d2 = if(n2 == 0) {
          2.0
        } else 
          2.0 - 3 * y * n3 / n2
        lazy val d3 = if(n3 == 0) {
          3.0
        } else 
          3.0 - 4 * y * n4 / n3
        
        var totalD = 0.0
        
        val discounteds = counts.map(c => {
          c match {
            case 0 => 0
            case 1 => {
              totalD += d1
              1 - d1
            }
            case 2 => {
              totalD += d2
              2 - d2
            }
            case _ => {
              totalD += d3
              c - d3
            }
          }
        }) 

        

      }
      */


      tags.foreach(sym => {
        val segs = nodeToSeg(rw,sym)

        var iProb : Double = 0.0
        //each segment e contributes some pMass to to n being taggen with sym
        segs.foreach(e => {        
          val leaves = walkWith(n,e.root) 
          
          if(leaves == null)
            throw new Exception()
          
          if(scoreMap(e) == 0)
            throw new Exception()


        
          val sProb = (scoreMap(e) /: leaves)(
            (a,l) => a * insideMap((new RefWrapper(l._1),l._2)) //the recursive call
          )
        
          iProb = iProb + sProb
        })

        //now iProb holds the inside probability of n and sym
        //this probability gets smoothed accross all categories

        val nTags = tags.length.toDouble
        tags.foreach(t => {
          val cur = insideMap.getOrElse((rw,t),0.0)
          if(t == sym) {
            insideMap += (rw,t) -> (cur + iProb * (1-smoothFac))
          } else {
            insideMap += (rw,t) -> (cur + (iProb * smoothFac / nTags))
          }
        })
      })
    }


    doinside(tree.root) //assumes that root is never split

    //println("INSIDE DONE")

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
          
          
          segs.foreach(e => {        

            //println(PCFGPrinter.treeToString(pcfg,e))


            val leaves = walkWith(n,e.root) 
            if(leaves == null)
              throw new Exception()
            
            //the outside prob at the leaf is prob(e) * outside at n * inside of other leaves
            var oProb = outsideMap(rw,sym) * scoreMap(e) 
            
            //val lIns = leaves.map(l => inside(l._1,l._2))
            
            leaves.foreach(l => {
              //println("LEAF - " + pcfg.symbolStrings(l._2))
              val myProb = (oProb /: leaves)((a,al) => {
                if(l eq al)
                  a
                else {
                  a * insideMap(new RefWrapper(al._1),al._2)
                }
              })
              
              val ltags = getTagged(pcfg.symbolStrings(l._1.symbol))
              ltags.foreach(t => {
                val key = (new RefWrapper(l._1),t)
                val cur = outsideMap.getOrElse(key,0.0)
                if(t == l._2) {
                  outsideMap += key -> (cur + myProb * (1-smoothFac))
                } else {
                  outsideMap += key -> (cur + (myProb * smoothFac / ltags.length.toDouble))
                }
              })
            })
          })          
        } else {
          /**
          outsideMap.keySet.elements.foreach(k => {
            println(pcfg.symbolStrings(k._2))
          })
          println(PCFGPrinter.treeToString(pcfg,tree))
          println(PCFGPrinter.nodeString(pcfg,n))
          throw new Exception("SHOULD ALWAYS BE AN OUSTSIDE!")
*/
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

    //holds the scores for each node as a list of tags and probability mass
    val scores = new HashMap[RefWrapper,List[Tuple2[ParseTypes.Symbol,Double]]]()

    
    def scoreTag(target : NonTerminalNode, segNode : NonTerminalNode, score : Double) : Unit = {
      
      val rw = new RefWrapper(target)
      var cur = scores.getOrElse(rw,Nil)

      //add score to target's taglist entry for segNode's tag
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

      
      //recurse to the rest of the segment
      segNode match {
        case un : UnderspecifiedNode => {
          //do nothing, prevents double counting
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

    
    //returns 10^-2 * P(least probable outside P)
    def unknownOut(n : NonTerminalNode) : Double = {
      println("UNK OUT!!!!")

      val tags = getTagged(pcfg.symbolStrings(n.symbol))
      
      var min = 1.0

      tags.foreach(t => {
        val oscr : Double = outsideMap.getOrElse((new RefWrapper(n),t),0)
        if(oscr > 0 && oscr < min)
          min = oscr
      })

      min * smoothFac
    }

    def unknownIn(n : NonTerminalNode) : Double = {
      println("UNK IN!!!")
      val tags = getTagged(pcfg.symbolStrings(n.symbol))
      
      var min = 1.0

      tags.foreach(t => {
        val oscr : Double = insideMap.getOrElse((new RefWrapper(n),t),0)
        if(oscr > 0 && oscr < min)
          min = oscr
      })

      min * smoothFac
    }


    //called recursively on every nonterminal node in the tree
    //gets all the segments that come off n and contributes their tag score
    def recScore(n : NonTerminalNode) : Unit = {
      val rw = new RefWrapper(n)

      val tags = getTagged(pcfg.symbolStrings(n.symbol))
      
      //for each possible tag for this node
      tags.foreach(sym => {
        //if some derivation gave node n the tag "sym" (o/w we never calculated the outside)
       
        val outscore = if(outsideMap.isDefinedAt((new RefWrapper(n),sym))) {
          val r = outsideMap(rw,sym)
          if(r == 0)
            unknownOut(n)
          else
            r

        } else {
          unknownOut(n)
        }

        val segs = nodeToSeg(rw,sym)
        segs.foreach(e => {        

          //the score from this segment is the segment's score times its insides and outside
          val leaves = walkWith(n,e.root)
            
          val inscores : List[Double] = leaves.map(l => {
            var inscr : Double = insideMap.getOrElse((new RefWrapper(l._1),l._2),0.0)
            if(inscr == 0.0)
              inscr = unknownIn(l._1)
            inscr
          })
          
          scoreTag(n,e.root,((scoreMap(e) * outscore) /: inscores)(_ * _))
        })
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


    //return best scoring tags for each node
    val ret = new HashMap[RefWrapper,ParseTypes.Symbol]()    
    tree.nonterminals.foreach(n => {
      val tags = scores(new RefWrapper(n))
      
      if(tags.length == 0)
        throw new Exception()

      //printNode(n)
      //println(tags)

      var maxV = -1.0
      var best : ParseTypes.Symbol = 0

      tags.foreach(_ match {
        case (t,d) => {
          
          if(d > maxV) {
            best = t
            maxV = d
          }

          //println(d + " - " + pcfg.symbolStrings(t) + " == " + pcfg.symbolStrings(best))
        }
      })
      
      ret += new RefWrapper(n) -> best

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
