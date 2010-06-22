package enbuske.parse.generate

import collection.mutable.HashMap
import scala.util.Random
import io.Source

object Jabberwock {

  def createFromPCFG(pcfg : PCFG) : Jabberwock = {
    var segmentMap = new HashMap[ParseTree,Double]();
    
	segmentMap ++= pcfg.rules.map(_ match {case (r,d) => 
		(new ParseTree(ProtoNode(r.lhs,r.rhs.map(t => UnderspecifiedNode(t,null)))),d)
	})
 
	segmentMap ++= pcfg.lexiconRules.map(_ match {
	  case (r,d) => (new ParseTree(PreTerminalNode(r.lhs,TerminalNode(r.terminal))),d)
	})
	new Jabberwock(pcfg,segmentMap)
  }
  
  def createFromFile(filename : String) : Jabberwock = {
    println("Loading Jabberwock from " + filename)
    val pcfg = new PCFG()
    val segmentMap = new HashMap[ParseTree,Double]();
    val filedata = Source.fromFile(filename).getLines
    val segments = filedata.map(_.split("\t")).toList
    println("Got " + segments.length + " segments")
    segments.map(a => {
      segmentMap += pcfg.growTree(a(0)) -> a(1).toDouble
    })
    new Jabberwock(pcfg,segmentMap) 
  }
}

class Jabberwock(val pcfg : PCFG, val segmentMap : HashMap[ParseTree,Double]) {
  
  /**
   * Used by Jabberwock only.
   */
  class TreeExtensionException extends Exception
  def extend(tree: ParseTree, 
             site : UnderspecifiedNode, 
             bottom : ParseTree) : ParseTree = {
    
    def recExtend(n : NonTerminalNode, bottom : NonTerminalNode, site : UnderspecifiedNode) : NonTerminalNode = {
      val ret = n match {
	    case PreTerminalNode(s,k) => PreTerminalNode(s,TerminalNode(k.terminal))
	    case pn : ProtoNode => ProtoNode(pn.symbol,pn.children.map(recExtend(_,bottom,site)))
	    case un : UnderspecifiedNode => {
	      if(un eq site){
	    	if(un.assignment != null)
	    	  throw new TreeExtensionException
	    	//shallow copies the tree in (good!)
	    	bottom
	      } else {
	    	if(un.assignment != null) 
	    	  UnderspecifiedNode(un.symbol,recExtend(un.assignment,bottom,site))
	    	else
	    	  UnderspecifiedNode(un.symbol,null)
	      }
	    }                             
      }
      ret
    }
    if(bottom.root.symbol != site.symbol)
      throw new TreeExtensionException
    new ParseTree(recExtend(tree.root,bottom.root,site))
  } 	 

  def toPCFG : PCFG = {
    val ret = new PCFG(pcfg) //create a PCFG with the right symbols
    type NTerm = ParseTypes.Symbol
    
    var ntCounts = HashMap[NTerm,Double]()
    ntCounts ++= pcfg.symbolIDs.map(_._2 -> 0.0)
    ntCounts += pcfg.symbolIDs("ROOT") -> 1.0
    var lastCounts = HashMap[NTerm,Double]()
    lastCounts ++= pcfg.symbolIDs.map(_._2 -> 0.0)
    
    def updateCounts() = {
      //for each nt, and for each segment headed by that nt
      val newCounts = HashMap[NTerm,Double]()
      newCounts ++= pcfg.symbolIDs.map(_._2 -> 0.0)
      newCounts += pcfg.symbolIDs("ROOT") -> 1.0
      ntCounts.foreach(_ match { 
        case (nt,count) => {
          //some nt's never start a segment and so will not be in headmap
          val ntOption = headMap.get(nt)
          if(ntOption.isDefined) {
	    	  ntOption.get.foreach(_ match {
	    	    case (seg,prob) => {
	    		  //update each leaf's count with ntcount * prob
	    		  val update = count * prob
	    		  //println("using segment")
	    		  //println(PCFGPrinter.treeToString(pcfg,seg))
	    		  //println("with update = " + update)
	    		  seg.underspecs.foreach(snt => 
	    		    newCounts += snt.symbol -> (newCounts.getOrElse(snt.symbol,0.0) + update))
	    	  }})
          }
          //println(newCounts.map(x => pcfg.symbolStrings(x._1.toInt) + " -> " + x._2))
        }
      })
      ntCounts = newCounts
    }
    
    def metric : Double = {
      val toSum = lastCounts.map(_ match {
        case (nt,count) => {
          Math.abs(count - ntCounts(nt))
        }   
      })
      val ret = (0.0 /: toSum)(_ + _)
      println("METRIC = " + ret)
      ret
    }
     
     
    
    val CONVERGE_THRESHOLD = .0001
    def converged_? : Boolean = metric < CONVERGE_THRESHOLD
    
    val MAX_ITER = 100
    var iter = 0
    println("Converting to PCFG")
    while(iter < MAX_ITER) {
      iter += 1
      if(converged_?) exit
      lastCounts = ntCounts
      updateCounts      
      ntCounts.foreach(_ match { case (nt,count) => println(pcfg.symbolStrings(nt.toInt) + " -> "  + count)})
    }
    
    //pull out the pcfg probabilities from the tsg
    
    //for each nonterminal, and each of its segments, record the counts of each rule
    //and totals for each nt
    val ruleCounts = HashMap[ProtoRule,Double]()
    val lexRuleCounts = HashMap[TerminalRule,Double]()
    val ruleTotals = HashMap[NTerm,Double]()
    ntCounts.foreach(_ match { 
      case (nt,count) => {
    	  val ntOption = headMap.get(nt)
          if(ntOption.isDefined) {
	    	  ntOption.get.foreach(_ match {
	    	    case (seg,prob) => {
	    	      val update = count * prob
	    	      seg.nonterminals.map(_.rule).foreach(_ match {
		    	      case tr : TerminalRule => {
		    	        lexRuleCounts += tr -> (lexRuleCounts.getOrElse(tr,0.0) + update)
		    	        val lhs = tr.lhs
		    	        ruleTotals += lhs -> (ruleTotals.getOrElse(lhs,0.0) + update)
		    	      } 
		    	      case pr : ProtoRule => {
		    	        ruleCounts += pr -> (ruleCounts.getOrElse(pr,0.0) + update)
		    	        val lhs = pr.lhs
		    	        ruleTotals += lhs -> (ruleTotals.getOrElse(lhs,0.0) + update)
		    	      }
		    	      case _ => {}
	    	      })
	    	    }})
          }
      }})        
       
    //derive the pcfg rules by dividing counts and totals
    ret.rules.clear
    ret.rules ++= ruleCounts.map(_ match {
      case (rule,count) => rule -> (count / ruleTotals(rule.lhs))  
    })
    ret.lexiconRules ++= lexRuleCounts.map(_ match {
      case (rule,count) => rule -> (count / ruleTotals(rule.lhs))  
    })
    ret
  }
  
  val myRand = util.Util.javaRandom  
  var headMap = new HashMap[ParseTypes.Symbol,List[Tuple2[ParseTree,Double]]]()
  segmentMap.foreach(_ match {case (ft,d) => {
	  val head = ft.root.symbol
	  var listo : List[Tuple2[ParseTree,Double]]= headMap.getOrElse(head,List())
	  headMap += (head -> ((ft,d) :: listo))
  }}) 
  
  //println(this.toString())
  override def toString() : String = {
    val segStrs = segmentMap.map(_ match {case (t,d) => PCFGPrinter.treeToString(pcfg,t) + "\t" + d.toString + "\n"})
    ("" /: segStrs)(_ + _)
  }
  
  def generateFrom(head : ParseTypes.Symbol) : ParseTree = {
	  var possibles = headMap(head)
	  var rando = myRand.nextDouble
	  for {(ft,d) <- possibles} {
		  rando -= d
		  if(rando <= 0) return ft
	  }
	  throw new Exception("Some numerical underflow?")
  }
  
  /**
   * Maximally unconstrained generation of a string of text
   */
  def speak() : String = {
    var tree = fillCanopy(genCanopy)
    PCFGPrinter.treeToString(pcfg,tree)
  }
  
  def generate_?(fTree : ParseTree, node : TreeNode) = {
      //should we generate a new node at node for fTree 
      true 
  }
  
  def genTree() : ParseTree = genTree(None)
  def genTree(maxGen : Option[Int]) : ParseTree = {
    fillCanopy(genCanopy(maxGen))
  }
  
  def genCanopy() : ParseTree = genCanopy(None)
  def genCanopy(maxGen : Option[Int]) : ParseTree = {
    var tree = generateFrom(ParseTypes.Root)
    var uspecs = tree.underspecs 
    var gen = 1
    while(!uspecs.isEmpty) {
      
      val site = uspecs(0)
      tree = extend(tree,site,generateFrom(site.symbol))
      gen += 1
      if(maxGen.isDefined && gen >= maxGen.get) {
        println("RESTART")
        return genCanopy(maxGen)
      }
      uspecs = tree.underspecs
    }
    tree
  }
  
  /**
   * For now, since canopies are fully generated, there is nothing to fill
   */
  def fillCanopy(fTree : ParseTree) : ParseTree = {
	fTree
  }
}
