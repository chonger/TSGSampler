package parse.tsg.train


//a real parse tree which hashes the same way as SegmentRefs
class MyHashTree(nt : NonTerminalNode) extends ParseTree(nt) {
  lazy val spHash = specialHash((nt : NonTerminalNode) => true)
  
  override def hashCode : Int = myHash
  
  override def equals(a : Any) : Boolean = {
     a match {
       case s : SegmentRef => s.equals(this) 
       case _ => super.equals(a)  
     }
  }
}

//An overlay of a segment on a tree with markers and one possible difference
class SegmentRef(root : NonTerminalNode, 
                 val tree : ParseTree with Markers, 
                 val markNode : NonTerminalNode, 
                 val isMarked : Boolean) extends ParseTree(root) {
  
  def pcfgScore(pcfg : PCFG) : Double = {
    def recScore(n : NonTerminalNode) : Double = {
      if(expand_?(n)) {
	      n match {
	      	case ptn : PreTerminalNode => {
	      	  //println(PCFGPrinter.ruleString(pcfg,ptn.rule) + " --> " + pcfg.lexiconRules(ptn.rule))
	      	  pcfg.lexiconRules(ptn.rule)
	      	}
	      	case un : UnderspecifiedNode => {
	      		if(un.assignment != null)
	      			recScore(un.assignment)
	      		else 
	      			1.0
	      	}
	      	case pn : ProtoNode => {
	      	  //println(PCFGPrinter.ruleString(pcfg,pn.rule) + " ==> " + pcfg.rules(pn.rule))
	      	  (pcfg.rules(pn.rule) /: pn.children)((a,b) => a * recScore(b))
	      	}
	      }
      } else 1.0
    }  
    val ret = recScore(root)
    //println("DONE")
    ret
  }
  
  lazy val spHash = specialHash(expand_?)
  override def hashCode : Int = dumpSegment().myHash
  
  //this will be compared with a real parse tree segment which is stored in a tree cache
  //
  override def equals(a : Any) : Boolean = {
    
    a match {
      case sr : SegmentRef => {
    	  //used to determine is two trees are equal for treeKronDel 
    	  (root eq sr.root) && (tree eq sr.tree) && (markNode eq sr.markNode) && (isMarked == sr.isMarked)
      }
      case pt : ParseTree => {
    	  //println("segEq")
    	  recCompare(root,pt.root)
      }
      case _ => false
    }
  }
  
  def recCompare(nt : NonTerminalNode, ref : NonTerminalNode) : Boolean = {
    val sameSym = (nt.symbol == ref.symbol)
    if(expand_?(nt)) { //if the current nt is expanded given the markings
		sameSym && (nt.children.length == ref.children.length) && (
		nt match {
		  case PreTerminalNode(s,k) => ref match {
		    case PreTerminalNode(ss,kk) => kk.terminal == k.terminal
		  }
		  case in : InternalNode => ref match {
		    case PreTerminalNode(s,k) => false
			case pn : ProtoNode => 
			  (true /: (in.children zip pn.children))((a,b) => a && recCompare(b._1,b._2))
			//if this node is expanded, the ref shouldnt be underspecified
			case un : UnderspecifiedNode => false
		  }
		})
    } else { //this node should be a underspec in the reference 
      sameSym && ref.isInstanceOf[UnderspecifiedNode]
    }
  }
  
  def expand_?(nt : NonTerminalNode) : Boolean = 
    if(nt eq markNode) !isMarked else if(nt eq root) true else !(tree.markers contains new RefWrapper(nt))
  
  override def recGetNodes(n : TreeNode) : List[TreeNode]= {
    
    n match { 
    	case nt : NonTerminalNode => {
    	  nt :: (if(expand_?(nt)) nt.children.flatMap(recGetNodes(_)) else Nil)
    	}
	    case _ => List(n)
	}
  }
  
  def dumpSegment() : MyHashTree = new MyHashTree(recDump(root))
  def recDump(n : NonTerminalNode) : NonTerminalNode = {
	  if(expand_?(n)) {
		  n match {
			  case BinaryNode(s,l,r) => BinaryNode(s,recDump(l),recDump(r))
			  case UnaryNode(s,k) => UnaryNode(s,recDump(k)) 
			  case PreTerminalNode(s,k) => PreTerminalNode(s,TerminalNode(k.terminal))
			  case pn : ProtoNode => ProtoNode(pn.symbol,pn.children.map(recDump(_)))
			  case un : UnderspecifiedNode => throw new Exception()
	  	  }
	  } else {
		  UnderspecifiedNode(n.symbol,null)
	  }
  }
  
}

trait Markers extends ParseTree {
  import scala.collection.mutable.{HashMap,HashSet}
  val markers = new HashSet[RefWrapper] //the markers define cut points in the tree
  def mark(n : NonTerminalNode) = {
    markers += new RefWrapper(n)
  }
  
  def unmark(n : NonTerminalNode) = {
    markers -= new RefWrapper(n)
  }
  
  def isMarked(n : NonTerminalNode) = {
    markers contains new RefWrapper(n)
  }
  
  def randomizeMarkers() : Unit = randomizeMarkers(.5)
  def randomizeMarkers(markProb : Double) : Unit = {
    println("RANDOMIZING MARKERS")
    val rando = butil.Util.javaRandom
    nonterminals.foreach(n => {
    	if(rando.nextDouble() <= markProb)
    		mark(n)
    	else
    		unmark(n)
    })
    mark(root)
    setupParents()
  }
  
  /**
   * Gets the tree of segments from the current tree as defined by the current markers
   */
  def getSegmentTree() : SegTree = {
      def recGetSegmentTree(n : NonTerminalNode) : SegTree = {
	      import scala.collection.mutable.{ListBuffer,HashMap}
		  var saved = new ListBuffer[NonTerminalNode]()
		  val treeroot = getSegmentFrom(n,
	                                 (ntn : NonTerminalNode) => {saved += ntn},
	                                 (n1 : NonTerminalNode, n2 : NonTerminalNode) => {})
		  new SegTree(new ParseTree(treeroot),saved.toList.map(recGetSegmentTree(_)))
	  }
	  recGetSegmentTree(root)
  }
  
  /**
   * Gets a list of tree segments from this tree, as defined by the current markers
   */
  def getSegments : List[ParseTree] = {
	  markers.map(m => new ParseTree(getSegmentFrom(m.n))).toList                                             
  }
  
  /**
   * markfn triggers on every leaf of the segment (= every marked non seg-root node in the segment)
   * linkfn triggers on every node
   */
  def getSegmentFrom(n : NonTerminalNode) : NonTerminalNode = 
    getSegmentFrom(n,(n) => {},(a,b) => {})
  def getSegmentFrom(n : NonTerminalNode,
                     linkfn : (NonTerminalNode,NonTerminalNode) => Unit) : NonTerminalNode = 
    getSegmentFrom(n,(n) => {},linkfn)
  def getSegmentFrom(m : NonTerminalNode, 
                     markfn : (NonTerminalNode) => Unit,
                     linkfn : (NonTerminalNode,NonTerminalNode) => Unit) : NonTerminalNode = {
      def recGetSeg(n : NonTerminalNode) : NonTerminalNode = {
		  val ret = 
			  if((!(m eq n)) && (markers contains new RefWrapper(n))) {
				  markfn(n)
				  UnderspecifiedNode(n.symbol,null)  
			  } else {
				  n match {
			  	  	case BinaryNode(s,l,r) => BinaryNode(s,recGetSeg(l),recGetSeg(r))
			  	  	case UnaryNode(s,k) => UnaryNode(s,recGetSeg(k)) 
			  	  	case PreTerminalNode(s,k) => PreTerminalNode(s,TerminalNode(k.terminal))
			  	  	case pn : ProtoNode => ProtoNode(pn.symbol,pn.children.map(recGetSeg(_)))
			  	  	case un : UnderspecifiedNode => {
			  	  		if(un.assignment != null) 
			  	  			UnderspecifiedNode(un.symbol,recGetSeg(un.assignment))
			  	  		else
			  	  			UnderspecifiedNode(un.symbol,null)
			  	  	}                             
				  }	
			  }
	   
		  linkfn(ret,n)
		  ret
      }
      recGetSeg(m)
  }
  
  /**
   * updates the parent link for a given node and all nodes below it which shared the same previous parent
   * 
   */
  def updateParent(nt : NonTerminalNode with Parent, parent : NonTerminalNode) : Unit = { 
    nt.parent = parent
    if(!isMarked(nt)) {
      nt match {
    	case ptn : PreTerminalNode => {}
      	case in : InternalNode => in.children.map(c => updateParent(c.asInstanceOf[NonTerminalNode with Parent],parent))
      }
    }
  }
  def updateParentsFromMarkAt(nt : NonTerminalNode) = {
    nt match {
    	case ptn : PreTerminalNode => {}
      	case in : InternalNode => in.children.map(c => updateParent(c.asInstanceOf[NonTerminalNode with Parent],nt))
    } 
  }
  def setupParents() = markers.foreach(m => updateParentsFromMarkAt(m.n))
        
  /**
   *   INITIALIZATION
   */
  
  markers += new RefWrapper(root) //the root is always marked
  setupParents()
  
  /**
   *
   *    BROKEN 
   * 
   */
  
  //maps a bank node ref to its parent segment in the cache and this node's site in that segment
  //val segmentMap = new HashMap[RefWrapper,Tuple2[NonTerminalNode,NonTerminalNode]]
  
  //import parse.tsg.TreeCache
  
  /**
   * DANGER!!!! broken
   * 
   * nt : is a node in the bank
   * tCache : is a tree cache of segments
   * 
   * return a the segment which extends from nt's parent, and the cache node corresponding to nt
   * in that segment
   */
  /**
  def getCachedParentSegSite(nt : NonTerminalNode, tCache : TreeCache) : Tuple2[ParseTree,NonTerminalNode]= {
	  val option = segmentMap.get(new RefWrapper(nt))
	  if(option.isDefined) {
		  val (proot,site) = option.get
		  (new ParseTree(proot),site) 
	  } else {
		  val parentNode = parentMap(new RefWrapper(nt))
		  var linkedNode : NonTerminalNode = null
		  def markfn = (leaf : NonTerminalNode) => {
		    if(leaf eq nt)
		    	linkedNode = leaf
		  }
		  val newSeg = getSegmentFrom(nt,markfn,(a,b) => {})
		  var cachedNode : NonTerminalNode = null
		  def cmarkfn = (leaf : NonTerminalNode) => {
		    if(leaf eq linkedNode)
		    	cachedNode = leaf
		  } 
		  //DANGER!!! this currently doesnt work, because the markfn will not find
		  val newSegTC = null //tCache.encodeNode(newSeg,cmarkfn)
		  segmentMap += new RefWrapper(nt) -> (newSegTC,cachedNode)
		  (new ParseTree(newSegTC),cachedNode)
	  }
  }
  */
    
  
}
