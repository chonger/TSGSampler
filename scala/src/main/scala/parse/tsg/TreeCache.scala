package parse.tsg

/**
 * After constructing a cache, the original data set should be garbage collected
 * 
 *
 */
class TreeCache() {
  import scala.collection.mutable.HashSet
  
  def this(data : Array[ParseTree]) = {
    this()
    data.foreach(encodeTree(_))
  }
  
  val cache = new HashSet[NonTerminalNode]()
  val size = cache.size 
  
  def encodeNode_![A <: NonTerminalNode](nt : A) : A = cache.findEntry(nt).get.asInstanceOf[A] 
  
  //makes sure the node is in the cache
  def encodeTree(tree : ParseTree) : ParseTree = {
    new ParseTree(encodeNode(tree.root)) 
  } 
   
  def encodeTree_!(tree : ParseTree) : ParseTree = {
    new ParseTree(encodeNode_!(tree.root))
  }
  
  
  def encodeNode[A <: NonTerminalNode](nt : A) : A = {
	val option  = cache.findEntry(nt)
    if(option.isDefined) 
      option.get.asInstanceOf[A]
    else {
	    nt match {
	      case in : InternalNode => {
	    	  in.children.foreach(encodeNode(_))
	    	  val ret = nt match {
	    	  	case BinaryNode(sym,left,right) => BinaryNode(sym,encodeNode(left),encodeNode(right))
	    	  	case UnaryNode(sym,kid) => UnaryNode(sym,encodeNode(kid))
	    	  	case pn : ProtoNode => ProtoNode(pn.symbol,pn.children.map(encodeNode(_)))
	    	  	case un : UnderspecifiedNode => encodeNode(un) 
	    	  }
	    	  cache += ret
	    	  ret.asInstanceOf[A]
	      }
	      case pt : TreeNode => {
	        if(!pt.isInstanceOf[PreTerminalNode])
	        	throw new Exception
	        cache += pt
	        encodeNode(pt).asInstanceOf[A]
	      }
	    }
     }
  }
}
