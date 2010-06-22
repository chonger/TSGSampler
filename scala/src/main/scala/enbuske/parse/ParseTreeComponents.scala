package enbuske.parse

/**
 * Copy a treenode and all of its children
 */
object TreeNode {
  def deepCopy(n : NonTerminalNode) : NonTerminalNode = {
	  n match {
	    case PreTerminalNode(s,k) => PreTerminalNode(s,TerminalNode(k.terminal))
	    case pn : ProtoNode => ProtoNode(pn.symbol,pn.children.map(deepCopy(_)))
	    case un : UnderspecifiedNode => {
	      if(un.assignment != null) 
	    	  UnderspecifiedNode(un.symbol,deepCopy(un.assignment))
	      else
	    	  UnderspecifiedNode(un.symbol,null)
        }                             
	  }
  }
  /**
  def addParentPtr(n : NonTerminalNode) : NonTerminalNode with Parent = {
	  n match {
	    case PreTerminalNode(s,k) => new PreTerminalNode(s,TerminalNode(k.terminal)) with Parent
	    case pn : ProtoNode => new ProtoNode(pn.symbol,pn.children.map(addParentPtr(_))) with Parent
	    case un : UnderspecifiedNode => {
	      if(un.assignment != null) 
	    	  new UnderspecifiedNode(un.symbol,addParentPtr(un.assignment)) with Parent
	      else
	    	  new UnderspecifiedNode(un.symbol,null) with Parent
        }                             
	  }
  }
  */
}

/**
 * Using the case class formalization will create hashCode and equals which apply to the whole tree
 */
abstract class TreeNode

/**
 * TreeNodes with this trait have pointers to the nodes which head
 * the elementary trees they are contained in (and not the root of)
 */ 
//trait Parent extends TreeNode {var parent : NonTerminalNode = null}

/**
 * TreeNodes with this trait have pointers to their head POS/word 
 */ 
trait Head extends TreeNode {var head : PreTerminalNode = null}

case class TerminalNode(val terminal : ParseTypes.Terminal) extends TreeNode
case class EmptyNode() extends TerminalNode(ParseTypes.Empty)
abstract case class NonTerminalNode(val symbol : ParseTypes.Symbol) extends TreeNode {
  def children : List[TreeNode] 
  def rule : TreeRule
}
case class PreTerminalNode(override val symbol : ParseTypes.Symbol,
						   val kid : TerminalNode) extends NonTerminalNode(symbol) {
	def children : List[TerminalNode]= List(kid)
	override def rule = new TerminalRule(symbol,kid.terminal)
}
abstract case class InternalNode(override val symbol : ParseTypes.Symbol) extends NonTerminalNode(symbol) {
	override def children() : List[NonTerminalNode]
}
case class UnderspecifiedNode(override val symbol : ParseTypes.Symbol,
							  val assignment : NonTerminalNode) extends InternalNode(symbol) {
  override def children = if(assignment == null) Nil else List(assignment)
  override def rule = null
}
case class ProtoNode(override val symbol : ParseTypes.Symbol,
                         val kids : List[NonTerminalNode]) extends InternalNode(symbol) {
	override def children = kids
	override def rule = new ProtoRule(symbol,kids.map(_.symbol))
}

//a wrapper for hashing based on pointer equality
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

/**
 * Rules, not case classed because they do not have recursive structure
 */
abstract class TreeRule(val lhs : ParseTypes.Symbol)
class ProtoRule(override val lhs : ParseTypes.Symbol, 
				val children : List[ParseTypes.Symbol]) extends TreeRule(lhs) {
	 def rhs = children
	 override def hashCode() : Int = {
		var shift = 0
	    (lhs.toInt /: children)((a,b) => {shift += 2; a + (b << shift)})
	 }
	 override def equals(any : Any) : Boolean = {
	  any match {
	    case u : ProtoRule => u.lhs == this.lhs && u.rhs == this.rhs
	    case _ => false	
	  }
	 }
}
class TerminalRule(override val lhs : ParseTypes.Symbol,
				   val terminal : ParseTypes.Terminal) extends TreeRule(lhs) {
	override def hashCode() : Int = {
	    lhs.toInt ^ (terminal << 8)
	 }
	 override def equals(any : Any) : Boolean = {
	  any match {
	    case u : TerminalRule => u.lhs == this.lhs && u.terminal == this.terminal
	    case _ => false	
	  }
	 }
} 
