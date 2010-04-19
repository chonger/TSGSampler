package parse

class TreeExtensionException extends Exception
/**
 *  The reason we need to wrap this object is that parse trees might have other annotations
 *  such as io probs or tree segmentations which are extraneous to their structure
 */
class ParseTree(val root : NonTerminalNode) {
	//TODO : does this deep copy??? investigate case classes
	def copy() = new ParseTree(root)
 
 	def deepCopy() = new ParseTree(TreeNode.deepCopy(root))
  
  	/**
     * these can be made unlazy if size gets to be a problem
     */
 	lazy val underspecs : List[UnderspecifiedNode] = 
 		(nodes filter (n => n.isInstanceOf[UnderspecifiedNode] && 
 			n.asInstanceOf[UnderspecifiedNode].assignment == null)).asInstanceOf[List[UnderspecifiedNode]]
 	lazy val nonterminals : List[NonTerminalNode] = (nodes filter (_.isInstanceOf[NonTerminalNode])).asInstanceOf[List[NonTerminalNode]]
 	lazy val terminals : List[TerminalNode] = (nodes filter (_.isInstanceOf[TerminalNode])).asInstanceOf[List[TerminalNode]]
  	lazy val nodes : List[TreeNode] = recGetNodes(root)
 	def recGetNodes(n : TreeNode) : List[TreeNode]= {
 	  n match { 
 	    case nt : NonTerminalNode => nt :: nt.children.flatMap(recGetNodes(_))
 	    case _ => List(n)
 	  }
 	}
  
 	def specifyProtoNodes() : ParseTree = new ParseTree(specifyProtoNodes(root))
 	 
 	def specifyProtoNodes(n : NonTerminalNode) : NonTerminalNode = {
 		n match {
 		  case un : UnderspecifiedNode => {
 		    if (un.assignment != null)
 		    	UnderspecifiedNode(un.symbol,specifyProtoNodes(un.assignment))
 		    else
 		    	UnderspecifiedNode(un.symbol,null)
 		
 		  }
 		  case ptn : PreTerminalNode => ptn
 		  case UnaryNode(sym,kid) => UnaryNode(sym,specifyProtoNodes(kid)) 
 		  case BinaryNode(sym,left,right) => BinaryNode(sym,
                                                        specifyProtoNodes(left),
                                                        specifyProtoNodes(right))
 		  case pn : ProtoNode => {
 		    val sym = pn.symbol
 		    val kids = pn.children
 		    kids.length match {
 		      case 1 => UnaryNode(sym,specifyProtoNodes(kids(0)))
 		      case 2 => BinaryNode(sym,
                              specifyProtoNodes(kids(0)),
 		      				  specifyProtoNodes(kids(1)))
 		      case _ => ProtoNode(sym,kids)
 		      
 		    }
 		  }
 		}
 	}
  
 
  
  def collapseUNodes() = new ParseTree(recCollapse(root))
  
  def recCollapse(n : NonTerminalNode) : NonTerminalNode = {
    n match {
      case BinaryNode(s,l,r) => BinaryNode(s,
                  recCollapse(l).asInstanceOf[NonTerminalNode],
                  recCollapse(r).asInstanceOf[NonTerminalNode])
      case UnaryNode(s,k) => UnaryNode(s,recCollapse(k).asInstanceOf[NonTerminalNode])
      case PreTerminalNode(s,k) => n
      case pn : ProtoNode => ProtoNode(pn.symbol,pn.children.map(c => recCollapse(c).asInstanceOf[NonTerminalNode]))
      case un : UnderspecifiedNode => {
        if(un.assignment == null)
          UnderspecifiedNode(un.symbol,null)
        else
          recCollapse(un.assignment)
      }
    }
  }
  
  /**
   * If site is not in this, returns this...this could be detected/avoided but at a runtime cost
   */
  def extend(site : UnderspecifiedNode, bottom : ParseTree) : ParseTree = 
    extend(site,bottom,(a,b) => {})
  def extend(site : UnderspecifiedNode, bottom : ParseTree,
             linkfn : (NonTerminalNode,NonTerminalNode) => {}) : ParseTree = {
    
    def recExtend(n : NonTerminalNode, bottom : NonTerminalNode, site : UnderspecifiedNode) : NonTerminalNode = {
    	val ret = n match {
	    	case BinaryNode(s,l,r) => BinaryNode(s,recExtend(l,bottom,site),recExtend(r,bottom,site))
	    	case UnaryNode(s,k) => UnaryNode(s,recExtend(k,bottom,site)) 
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
    	linkfn(ret,n)
    	ret
    }
    if(bottom.root.symbol != site.symbol)
      throw new TreeExtensionException
    new ParseTree(recExtend(root,bottom.root,site))
  }
  
  def sentence(pcfg : PCFG) : String = {
    val yld = nodes filter (n => n.isInstanceOf[TerminalNode] || 
                              (n.isInstanceOf[UnderspecifiedNode] && 
                               n.asInstanceOf[UnderspecifiedNode].assignment == null))
    
    ("" /: yld)((a,b) => {
                            var str = b match {
                            	case TerminalNode(sym) => pcfg.terminalStrings(sym)
                            	case UnderspecifiedNode(sym,asgn) => "*" + pcfg.symbolStrings(sym) + "*"
    						}
                            a + " " + str
    })
  }
  
 	override def equals(any : Any) : Boolean = {
 	  //println("calling ParseTree equals")
 	  any match {
 	    case s : tsg.train.SegmentRef => s.equals(this)
	    case t : ParseTree => {
	      if(hashCode == t.hashCode)
	    	  return t.root == this.root
	      false
	    }
	    case _ => false
	  }
	}
    lazy val myHash = root.hashCode
 	override def hashCode() : Int = myHash
  
 	def specialHash(expandme : (NonTerminalNode) => Boolean) : Int = {
 		//println("Special Hash")
 		var offset = 0
 		def getHash(nt : NonTerminalNode) : Int = { 
	    	val recHash : Int = if(expandme(nt)) 
	    		nt match {
	    		case BinaryNode(s,l,r) => getHash(l) ^ getHash(r)
	    		case UnaryNode(s,k) => getHash(k) 
	    		case PreTerminalNode(s,k) => getHashT(k)
	    		case pn : ProtoNode => (0 /: pn.children)(_ ^ getHash(_))
	    		case un : UnderspecifiedNode => {
	    			if(un.assignment != null) 
	    				getHash(un.assignment)
	    			else
	    				1
	    		}     
	    	} else 1
	    	val retHash = recHash ^ ((nt.symbol.toInt + 2) << offset)
	    	offset += 3
	    	offset = offset % 32
	    	//val retHash = (recHash * 29) * (nt.symbol.toInt)
	    	retHash
	    }
	    def getHashT(tn : TerminalNode) : Int = tn.terminal.toInt + 2
	 
	    val hash = getHash(root)
	    //println("RESULT = " + hash)
	    hash
 	} 
}

class RefWrapper(val n : NonTerminalNode) {
	override def equals(any : Any) : Boolean = {
			any match {
			case t : RefWrapper => {
				return n eq t.n
			}
			case _ => false
			}	
	}
	override def hashCode() : Int = n.hashCode()
}

class FullSpecException extends Exception
trait FullSpec extends ParseTree {
  if(nodes.filter(_.isInstanceOf[UnderspecifiedNode]).length > 0)
    throw new FullSpecException
}



