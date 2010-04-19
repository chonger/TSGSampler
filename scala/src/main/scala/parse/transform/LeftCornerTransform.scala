package parse.transform

import collection.mutable.Stack
import util.matching.Regex
//does left corner transform, with null emissions
 
abstract class LabelTransformer {
  def contains(s : String) : Boolean
  def symbol(s : String) : String
}

class RXLabelTransformer(val rxstr : String, val symf : String => String) extends LabelTransformer {
  val rx = new Regex(rxstr)
  override def contains(s : String) : Boolean = {
    val res = rx.findFirstIn(s)      
    res .isDefined
  }
  override def symbol(s : String) = {
    symf(s)
  } 
}

class LCRXform(rxstr : String, repl : String) extends RXLabelTransformer(rxstr,(s : String) => {
			 repl + "lc" + new Regex(rxstr).findFirstMatchIn(s).get.group(1)
})
 
trait LeftCornerTransform extends PCFG {
  /**
	private var labelTransformers : List[LabelTransformer] = List[LabelTransformer]()  
	  
	labelTransformers ::= new LCRXform("^.*lc(.*)$","XX")
	labelTransformers ::= new LCRXform("^(?:S|SBAR|SINV|SQ)lc(.*)$","XS")
	labelTransformers ::= new LCRXform("^(?:VP)lc(.*)$","XV")
	labelTransformers ::= new LCRXform("^(?:PP|ADVP|ADJP)lc(.*)$","XP")
	labelTransformers ::= new LCRXform("^(?:NP|LST|UCP|QP)lc(.*)$","XN")
	  
	def lcVerbose(s : String) = println(s)
	  
	abstract override def transform(data : List[ParseTreeData]) = {
		super.transform(data)
		var i = -1
		i = i + 1
		data.foreach(d => {
			try {		
				d.tree.root = recLC(d.tree.root)
			} catch {
				case e : Exception => {println("Failed to Left Corner tree " + i)}
			}
		})	
	}	
	abstract override def revert(data : List[ParseTreeData]) = {
		var i = -1
		data.foreach(d => {
			i = i + 1
			try {	
				d.tree.root = revLC(d.tree.root)
			} catch {
				case e : Exception => {
				  println("Failed to Reverse Left Corner tree " + i)
				}
			}
		})
		super.revert(data)
	}
   
  	def revLC(n : NonTerminalNode) : NonTerminalNode = {
  	  
  	  if(n.symbol.isInstanceOf[Root]) {
  		  var node =  UnaryTreeNode(Root(),revLC(n.children.asInstanceOf[List[NonTerminalNode]](0)))
  		  
  		  return UnaryTreeNode(Root(),node.kid.asInstanceOf[UnaryTreeNode].kid)
  	  }
      
  	  
  	  //preterminal nodes do not get transformed
  	  if(n.isInstanceOf[PreTerminalNode]) {
  		  return n
  	  } 
  	  
  	  var stack = new Stack[NonTerminalNode]()
  	  
  	  var cur = n
  	  while(!cur.isInstanceOf[PreTerminalNode]) {
  	    stack.push(cur)   
  	    cur = cur match {
  	  		case BinaryTreeNode(s,left,right) => right 
  	  		case UnaryTreeNode(s,kid) =>  kid
  	  		case ProtoTreeNode(s,kids) => kids.last
  	    }
  	  }
  	  stack.pop
  	  var ret = readSpine(stack,n.symbol)
  	  
  	  ret
  	}
 
   	def readSpine(stack : Stack[NonTerminalNode], n : NonTerminal) : NonTerminalNode = {
   	  var cur = stack.top
      stack.pop
      
      if(stack.isEmpty) {
    	  cur match {
   		  	case BinaryTreeNode(s,left,right) => left 
   		  }
      } else {
    	  var (s1,s2) = splitLCSymbol(cur.symbol)
    	  cur match {
    	  	case BinaryTreeNode(s,left,right) => new BinaryTreeNode(n,readSpine(stack,s2),revLC(left)) 
   		  	case UnaryTreeNode(s,kid) =>  new UnaryTreeNode(n,readSpine(stack,s2))
   		  	case ProtoTreeNode(s,kids) => new ProtoTreeNode(n,
                         readSpine(stack,s2) :: kids.reverse.drop(1).reverse.map(revLC(_)))
    	  } 
      }
   	} 
   
   	
   	def splitLCSymbol(n : NonTerminal) = {
   	  var strs = symbolStrings(n.id).split("lc")
      var nt1 = try {
        NonTerminal(symbolIDs(strs(0)))
      } catch {
        case e : NoSuchElementException => null
      }
      
      var nt2 = if(strs.length > 1) NonTerminal(addSymbol(strs(1))) else null
      (nt1,nt2)
   	}
   
  	def recLC(n : NonTerminalNode) : NonTerminalNode = {
  	  	
  	 	if(n.symbol.isInstanceOf[Root]) {
  	 		return UnaryTreeNode(n.symbol,
                           recLC(new UnaryTreeNode(NonTerminal(addSymbol("RS")),
                                                   n.children.asInstanceOf[List[NonTerminalNode]](0))))
      
  	 	}
        
  	 	if(n.isInstanceOf[PreTerminalNode]) {
  	 		return n
  	 	}
  
  	 	var stack = new Stack[NonTerminalNode]()
  	 	def findLeftCorner(n : NonTerminalNode) : PreTerminalNode = {
  	 		stack.push(n)
  	 		var ret = n match {
  	 			case pt : PreTerminalNode => pt
  	 			case BinaryTreeNode(s,left,right) => findLeftCorner(left) 
  	 			case UnaryTreeNode(s,kid) =>  findLeftCorner(kid)
  	 			case ProtoTreeNode(s,kids) => findLeftCorner(kids(0).asInstanceOf[NonTerminalNode])
  	 		}
  	 		ret
  	 	}
  	 	var leftCorner = findLeftCorner(n)
  	 	new BinaryTreeNode(n.symbol,leftCorner,buildSpine(n.symbol,stack)) 
  	}  
   
   	def buildSpine(sym : NonTerminal, stack : Stack[NonTerminalNode]) : NonTerminalNode = {
   	  	
	  	if(stack.size == 1) {
	  		var newSym = combineSymbols(sym,sym)
	  		return UnaryTreeNode(newSym,new PreTerminalNode(NonTerminal(addSymbol("EPSILON")),EmptyNode()))
	  	}
	  	var newSym = combineSymbols(sym,stack.top.symbol)
	  	stack.pop
	  	stack.top match {
	  	  case pt : PreTerminalNode => throw new Exception("Shouldn't happen")
	  	  case BinaryTreeNode(s,left,right) => {
	  		  var newLeft = recLC(right)
	  		  new BinaryTreeNode(newSym,newLeft,buildSpine(sym,stack))
	  	  }
	  	  case UnaryTreeNode(s,kid) => {
	  	      new UnaryTreeNode(newSym,buildSpine(sym,stack))
	  	  }
	  	  case ProtoTreeNode(s,kids) => {
	  		  var newKids = kids.drop(1).map(n => recLC(n.asInstanceOf[NonTerminalNode]))
	  		  newKids ++= List(buildSpine(sym,stack))
	  		  new ProtoTreeNode(newSym,newKids)
	  	  }
	  	}
   	}
    
    def combineSymbols(base : NonTerminal, other : NonTerminal) : NonTerminal = {
      	var s = symbolStrings(base.id)
      	s = s + "lc" + symbolStrings(other.id)
        
      	labelTransformers.foreach(xfrmr => {
        	if(xfrmr.contains(s)) {
        		return NonTerminal(addSymbol(xfrmr.symbol(s)))
        	}
        })	
        
	 	NonTerminal(addSymbol(s))
    }
    */
}
