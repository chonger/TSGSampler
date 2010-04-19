package parse.tsg.train

import java.io.{DataOutputStream,FileOutputStream,BufferedOutputStream,File}
import java.io.{DataInputStream,FileInputStream,BufferedInputStream}

class TSGPackager {

  import scala.collection.mutable.HashMap

  val rulemap = new HashMap[TreeRule,Int]() //record the enumeration of all rules
  var lhsOfRule : Array[Int] = null //record the index of a rule's lhs
  var pcfgProbs : Array[Double] = null


  def packageTrainer(pcfg : PCFG, trainer : TSGTrainer,filename : String) {
    
    println("PACKAGING")

    val dos = new DataOutputStream(
      new BufferedOutputStream(new FileOutputStream(new File(filename))))

    /**
     * to package -
     *
     *    length = numRules
     *    rhsMap - for each enumerated rule, the index of its rhs
     *    pcfg - the pcfg prob for each enumerated rule
     *
     *    beta - betas (length = num rhs)
     *    alpha - (length = num rhs)
     *    trees - put into struct format
     *
     */

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

    println("NUM LHS = " + (pcfg.nextSymID.toInt + 1))

    lhsOfRule.foreach(lhs => {
      if(lhs >= (pcfg.nextSymID.toInt + 1))
        throw new Exception("Bad lhs = " + lhs)
    })

    dos.writeInt(pcfg.nextSymID + 1)
    trainer.betas.foreach(d => dos.writeDouble(d))
    trainer.alphas.foreach(d => dos.writeDouble(d))

    dos.writeInt(trainer.data.length)
    println("NUM TREES = " + trainer.data.length)
    trainer.data.foreach(writeTree(_,dos))
    
    dos.close
  }

  
  def writeTree(tree : ParseTree with Markers, dos : DataOutputStream) : Unit = {
    
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
	      throw new Exception("Shouldnt find underspec nodes")
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
          
          var sibs = pn.children.map((n) => true).toArray
          sibs(sibs.length - 1) = false
          var pind = index
          (pn.children zip sibs.toList).foreach(_ match {case (c,s) => walktree(c,pind,s)})
        }
	    case un : UnderspecifiedNode => {
	      throw new Exception("Shouldnt find underspec nodes")
        }                             
	  }
    }

    walktree(tree.root,0,false)

    markers.foreach(m => {dos.writeBoolean(m)})


  }

  

  def unpack(pcfg : PCFG, data : Array[ParseTree with Markers], infile : String) : PTSG = {
    val dis = new DataInputStream(
      new BufferedInputStream(new FileInputStream(new File(infile))))

    val nTrees : Int = dis.readInt()
    
    println("got data for " + nTrees + " trees")

    for{i <- 0 to (nTrees - 1)}{
    
      val nNodes : Int = dis.readInt() 

      //println("This tree has " + nNodes + "nodes")

      val nts = data(i).nonterminals //gets nts in dfs order
      


      for{j <- 0 to nNodes - 1} {
        if(dis.readByte() > 0) {
          data(i).mark(nts(j))
        }
      }
    }

    val nLHS = dis.readInt()
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

}
