package enbuske.parse.tsg

import java.io.{DataOutputStream,FileOutputStream,BufferedOutputStream,File}
import java.io.{DataInputStream,FileInputStream,BufferedInputStream}
import util.LexicalHeads

class TSGPackager(val pcfg : PCFG) {

  val nLHS = pcfg.nextSymID.toInt + 1

  import scala.collection.mutable.HashMap

  val rulemap = new HashMap[TreeRule,Int]() //record the enumeration of all rules
  var lhsOfRule : Array[Int] = null //record the index of a rule's lhs
  var pcfgProbs : Array[Double] = null

  def isTagged_?(n : NonTerminalNode) : Boolean = {
    val str = pcfg.getSym(n)
    if(str == "-LRB-" || str == "-RRB-")
      return false
    if(str.indexOf("-") > 0)
      return true
    false
  }

  /**
   * Must specify
   * each tree set
   * number of derived DP's
   * for each tree set specify connections
   *
   *
   */ 
  def packageHDP(data : List[List[ParseTree with Markers]], 
                 numDP : Int,
                 dpConnect : List[List[Double]],
                 filename : String) = {
    
    val dos = new DataOutputStream(
      new BufferedOutputStream(new FileOutputStream(new File(filename))))

    println("PACKAGING HDP")

    packagePCFG(dos)
    
    dos.writeInt(data.length) //number of tree sets

    var ind = 0
    data.foreach(tset => {
      packageTrees(dos,tset,ind)
      ind += 1
    })

    
    //HDP Loads from here

    val betas = for(i <- 1 to nLHS) yield .5
    val alphas = for(i <- 1 to nLHS) yield 100

    //base DP info
    betas.foreach(d => dos.writeDouble(d))
    alphas.foreach(d => dos.writeDouble(d))

    println(numDP + " Dirichlet Processes")
    //write number of DPs
    dos.writeInt(numDP)

    //alphas for each DP
    for{i <- 1 to numDP} {
      alphas.foreach(d => dos.writeDouble(d))
    }

    println("Connection matrix")
    //for each treeset write the mixture of each dp for each 
    dpConnect.foreach(cList => {
      cList.foreach(c => {
        print(c + "\t")
        for(i <- 1 to nLHS) {dos.writeDouble(c)}
      })      
      println()
    })

    dos.close
  }


  def packageOne(dataIn : List[ParseTree with Markers], filename : String) = {

    val dos = new DataOutputStream(
      new BufferedOutputStream(new FileOutputStream(new File(filename))))

    packagePCFG(dos)
    packageTrees(dos,dataIn)

    val betas = for(i <- 1 to nLHS) yield .5
    val alphas = for(i <- 1 to nLHS) yield 100

    alphas.foreach(d => dos.writeDouble(d))
    betas.foreach(d => dos.writeDouble(d))
    
    dos.close
  }

  def packagePCFG(dos : DataOutputStream) = {
    var index = 0
    var probs : List[Double] = Nil
    var lhsL : List[Int] = Nil
    var lhsIndex = -1

    pcfg.rules.foreach(rule => {
      rulemap += (rule._1 -> index)
      probs ::= rule._2
      lhsL ::= rule._1.lhs
      index += 1
    })
    pcfg.lexiconRules.foreach(rule => {
      rulemap += (rule._1 -> index)
      probs ::= rule._2
      lhsL ::= rule._1.lhs
      index += 1
    })
    
    pcfgProbs = probs.reverse.toArray
    lhsOfRule = lhsL.reverse.toArray

    //write number of rules
    println("NUM RULES = " + pcfgProbs.length)

    dos.writeInt(pcfgProbs.length)
    
    pcfgProbs.foreach(d => {dos.writeDouble(d)})
    
    lhsOfRule.foreach(i => {dos.writeInt(i)})

    dos.writeInt(pcfg.nextSymID + 1)
  }

  def packageTrees(dos : DataOutputStream, 
                   dataIn : List[ParseTree with Markers]) : Unit = {
    packageTrees(dos,dataIn,0)
  }

  def packageTrees(dos : DataOutputStream, 
                   dataIn : List[ParseTree with Markers],
                   aspect : Int) : Unit = {
    
    var throwout = 0
    
    val data1 = dataIn.filter(tree => {
      val l = tree.nonterminals.length
      if(l > 255) {
        throwout += 1
        false
      } else
        true
    })

    println("Threw out " + throwout + " too big trees")

    var data = data1.map(t => new ParseTree(t.root) with Markers with LexicalHeads)
    data.foreach(d => d.setHeads(pcfg))

    dos.writeInt(data.length)
    println("NUM TREES = " + data.length)
    data.foreach(t => {
      if(t.nonterminals.length > 255)
        throw new Exception()
      writeTree(t,dos,aspect)
    })
  }

  def writeTree(tree : ParseTree with Markers with LexicalHeads, dos : DataOutputStream, aspect : Int) : Unit = {
    
    val nts = tree.nonterminals
    val numNodes : Short = tree.nonterminals.length.toShort
    dos.writeInt(numNodes)
    //println("Writing " + numNodes + " nodes")

    val markers = new Array[Boolean](numNodes);
    var index = -1

    /**
     * for each node, we need
     * index of rule
     * is Terminal
     *
     * shorts
     * offset to head (assume only root is marked for now), head = index
     * offset to parent (neg)
     * offset to sibling (pos)
     *
     * if using lexical heads
     * offset to lexical head (pos)
     * 
     */
 
    def countUnder(n : NonTerminalNode) : Int = {
      n match {
	    case tn : PreTerminalNode => {
          1
        }
	    case pn : ProtoNode => {
          (1 /: pn.children)(_ + countUnder(_))          
        }
	    case un : UnderspecifiedNode => {
	      throw new Exception("Shouldnt find underspecified nodes")
        }                             
	  }
    }

    def walktree(n : NonTerminalNode,parentOff : Int,hasSibling : Boolean) : Unit = {
      index += 1

      if(tree.markers contains new RefWrapper(n))
        markers(index) = true
      else
        markers(index) = false
	  n match {
	    case tn : PreTerminalNode => {
          val rule = tn.rule
          dos.writeInt(rulemap(rule))
          dos.writeBoolean(true)
          dos.writeInt(index)
          dos.writeInt(index - parentOff)
          if(hasSibling)
            dos.writeInt(1) //next node must be sibling
          else
            dos.writeInt(0)
          dos.writeInt(index) //a preterminal is automatically its own head word
          if(isTagged_?(tn))
            dos.writeInt(1)
          else
            dos.writeInt(0)
          dos.writeInt(aspect) //aspect
          dos.writeInt(0) //foot
        }
	    case pn : ProtoNode => {
          val rule = pn.rule
          dos.writeInt(rulemap(rule))
          dos.writeBoolean(false)
          dos.writeInt(index)
          dos.writeInt(index - parentOff)
          if(hasSibling) {
            val numUnder = countUnder(pn)
            dos.writeInt(numUnder) //numUnder counts this node as well
          } else
            dos.writeInt(0)
          
          //what is the offset of this node's head?
          val headPT = tree.getHead(pn)
          var hInd = 0
          while(!(headPT eq nts(hInd))) hInd += 1
          dos.writeInt(hInd)

          var hasTag = isTagged_?(pn)
          hasTag = (hasTag /: pn.children)((a,b) => a || isTagged_?(b))
          if(hasTag) 
            dos.writeInt(1)
          else
            dos.writeInt(0)

          dos.writeInt(aspect) //aspect
          dos.writeInt(0) //foot

          var sibs = pn.children.map((n) => true).toArray
          sibs(sibs.length - 1) = false
          var pind = index
          (pn.children zip sibs.toList).foreach(_ match {case (c,s) => walktree(c,pind,s)})
        }
	    case un : UnderspecifiedNode => {
	      throw new Exception("Shouldnt find underspecified nodes")
        }                             
	  }
    }
    
    walktree(tree.root,0,false)
    
    markers.foreach(m => {dos.writeBoolean(m)})
  }  
  
  def unpack(data : Array[ParseTree with Markers], infile : String) : PTSG = {
    val dis = new DataInputStream(
      new BufferedInputStream(new FileInputStream(new File(infile))))

    //burn through a bunch of data
    val nRules = dis.readInt()
    for{i <- 1 to nRules}{dis.readDouble}
    for{i <- 1 to nRules}{dis.readInt}

    val nLHS = dis.readInt()
    

    val nTrees : Int = dis.readInt()
    
    //println("got data for " + nTrees + " trees")

    for{i <- 0 to (nTrees - 1)}{
    
      val nNodes : Int = dis.readInt() 

      //burn node data
      for{i <- 1 to nNodes}{
        dis.readInt()
        dis.readByte()
        dis.readInt()
        dis.readInt()
        dis.readInt()
        dis.readInt()
        dis.readInt()
      }

      //Node data is stored in DFS order
      val nts = data(i).nonterminals 

      for{j <- 0 to nNodes - 1} {
        if(dis.readByte() > 0) {
          data(i).mark(nts(j))
        }
      }
    }

    val alphas = (for{i <- 1 to nLHS} yield {
      dis.readDouble()
    }).toArray    
    val betas = (for{i <- 1 to nLHS} yield {
      dis.readDouble()
    }).toArray



    val counts = new HashMap[ParseTree,Int]()

    data.foreach(tree => {
      tree.getSegments.foreach(seg => {
        val ent = counts.getOrElse(seg,0) + 1
        counts += seg -> ent
      })
    })
    
    val dist = new CohnGoldwater(pcfg,betas)

    val ptsg = new PTSG(pcfg,counts,dist,alphas)
    /**
    ptsg.counts.elements.filter(_._2 > 20).foreach(_ match {
      case (a,b) => {println(PCFGPrinter.treeToString(pcfg,a)); println(b)}
    })
    */

    ptsg
  }

  def unpackHDP(data : List[Array[ParseTree with Markers with Aspect]], infile : String) : HDPTSG = {
    val dis = new DataInputStream(
      new BufferedInputStream(new FileInputStream(new File(infile))))

    //burn through the PCFG, just make sure its set up in the same way
    val nRules = dis.readInt()
    for{i <- 1 to nRules}{dis.readDouble}
    for{i <- 1 to nRules}{dis.readInt}
    val nLHS = dis.readInt()    

    val numTSets = dis.readInt()
    for(i <- 0 to numTSets-1) {
      val nTrees : Int = dis.readInt()
      println("READING " + nTrees + " TREES")
      for(k <- 0 to nTrees - 1) {
        val pt = data(i)(k)
        val nNodes : Int = dis.readInt()  //burn number of nodes
        pt.nonterminals.foreach(n => {
      
      
          dis.readInt()
          dis.readByte()
          dis.readInt()
          dis.readInt()
          dis.readInt()
          dis.readInt()
          dis.readInt()
          val asp = dis.readInt()
          dis.readInt()

          pt.aspect += (new RefWrapper(n) -> asp)
        })

        //Node data is stored in DFS order
        val nts = pt.nonterminals 
      
        for{j <- 0 to nNodes - 1} {
          if(dis.readByte() > 0) {
            pt.mark(nts(j))
          }
        }
      }
    }
    
    val betas = (for{i <- 1 to nLHS} yield {
      dis.readDouble()
    }).toArray

    val baseAlpha = (for{i <- 1 to nLHS} yield {
      dis.readDouble()
    }).toArray    
    
    val numDP = dis.readInt()
  
    val mixAlphas = (for{i <- 1 to numDP} yield {
      (for{i <- 1 to nLHS} yield {
        dis.readDouble()
      }).toArray    
    }).toArray

    
    var mixWeights :Array[Array[Array[Double]]] = new Array[Array[Array[Double]]](numTSets,numDP,nLHS)

    for(i <- 0 to numTSets - 1) {
      println(i)
      for(j <- 0 to numDP - 1) {
        println(j)
        for(k <- 0 to nLHS - 1) {
          mixWeights(i)(j)(k) = dis.readDouble
          println(k + "-" + mixWeights(i)(j)(k))
        }
      }
    }

    val mixCounts = (for(i <- 1 to numDP) yield {
      new HashMap[ParseTree,Int]()
    }).toArray

    val counts = new HashMap[ParseTree,Int]()

    val dist = new CohnGoldwater(pcfg,betas) //we need this to fill the base


    //ripped out of hdptsg
    /**
    val headMap = (for{i <- 0 to pcfg.nextSymID} yield {
      Nil
    }).toArray
    import scala.collection.mutable.HashSet
    val segSet = new HashSet[ParseTree]()
    data.foreach(tree => {
      tree.getSegments.foreach(seg => {
        if(!segSet.contains(seg)) {
          headMap(seg.root.symbol) ::= seg
          segSet += seg
        }
      })
    })
    */

    //headTotals is a list of the total counts of trees 
    val headTotals = (for{i <- 0 to numDP - 1} yield {
      //for each DP, start with 0 count
      (for{i <- 0 to pcfg.nextSymID} yield {0}).toArray
    }).toArray

    val baseTotals = (for{i <- 0 to pcfg.nextSymID} yield {0}).toArray


    //populate
    data.foreach(_.foreach(tree => {
   
	  tree.markers.map(m => {
        val seg = new ParseTree(tree.getSegmentFrom(m.n))
        val aspect = tree.aspect(new RefWrapper(m.n))
        var mixmap = mixCounts(aspect)
        val prev = mixmap.getOrElse(seg,0)
        
        val sym = seg.root.symbol

        //with some probability, we will also insert this into the baseCounts

        val baseScore = (baseAlpha(sym) * dist.score(tree) + counts.getOrElse(seg,0)) / (baseAlpha(sym) + baseTotals(sym))

        val fromTable = (prev) / (mixAlphas(aspect)(sym) + headTotals(aspect)(sym))

        val fromBase = (mixAlphas(aspect)(sym) * baseScore) / (mixAlphas(aspect)(sym) + headTotals(aspect)(sym))

        val ratio = fromTable / (fromTable + fromBase)

        val rando = new java.util.Random()
        if(rando.nextDouble() > ratio) {
          //sit at a new table
          val bPrev = counts.getOrElse(seg,0)
          counts += seg -> (bPrev + 1)
        }

        val next = prev + 1
        mixmap += seg -> next
      })
    }))

    val hdptsg = new HDPTSG(pcfg,counts,baseAlpha,mixCounts,mixAlphas,mixWeights,dist)

    hdptsg
  }

}
