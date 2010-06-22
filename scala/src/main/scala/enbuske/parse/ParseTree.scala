package enbuske.parse

/**
 *  The reason we need to wrap this object is that parse trees might have other annotations
 *  such as io probs or tree segmentations which are extraneous to their structure
 */
class ParseTree(val root : NonTerminalNode) {
 
  def deepCopy() = new ParseTree(TreeNode.deepCopy(root))
  
  lazy val nodes : List[TreeNode] = recGetNodes(root)
  def recGetNodes(n : TreeNode) : List[TreeNode]= {
 	n match { 
 	  case nt : NonTerminalNode => nt :: nt.children.flatMap(recGetNodes(_))
 	  case _ => List(n)
 	}
  }
  
  lazy val underspecs : List[UnderspecifiedNode] = 
 	(nodes filter 
     (n => n.isInstanceOf[UnderspecifiedNode] && 
 	  n.asInstanceOf[UnderspecifiedNode].assignment == null)).asInstanceOf[List[UnderspecifiedNode]]
  
  lazy val nonterminals : List[NonTerminalNode] = 
    (nodes filter (_.isInstanceOf[NonTerminalNode])).asInstanceOf[List[NonTerminalNode]]
  
  lazy val terminals : List[TerminalNode] = 
    (nodes filter (_.isInstanceOf[TerminalNode])).asInstanceOf[List[TerminalNode]]
  
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
  
  /**
   *          HASH FUNCTION
   */ 
  override def equals(any : Any) : Boolean = {
 	  //println("calling ParseTree equals")
 	any match {
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
}


