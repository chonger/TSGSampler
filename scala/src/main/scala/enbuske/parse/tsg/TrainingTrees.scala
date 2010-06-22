package enbuske.parse.tsg

/**
 * 
  * 
  * 
 **/ 

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
    val rando = util.Util.javaRandom
    nonterminals.foreach(n => {
    	if(rando.nextDouble() <= markProb)
    		mark(n)
    	else
    		unmark(n)
    })
    mark(root)
  }

  /**
   * Gets a list of tree segments from this tree, as defined by the current markers
   */
  def getSegments : List[ParseTree] = {
	  markers.map(m => new ParseTree(getSegmentFrom(m.n))).toList                                             
  }
  
  def getSegmentFrom(m : NonTerminalNode) : NonTerminalNode = {  
      def recGetSeg(n : NonTerminalNode) : NonTerminalNode = {
		  val ret = 
			  if((!(m eq n)) && (markers contains new RefWrapper(n))) {
				  UnderspecifiedNode(n.symbol,null)  
			  } else {
				  n match {
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
		  ret
      }
      recGetSeg(m)
  }

  /**
   *   INITIALIZATION
   */
  
  markers += new RefWrapper(root) //the root is always marked
}

