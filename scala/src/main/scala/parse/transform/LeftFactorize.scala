package parse.transform

import collection.mutable.Stack

abstract trait LeftFactorizeBar extends {
/**  
	abstract override def transform(data : List[ParseTreeData]) = {
		super.transform(data)
		var i = -1
		data.foreach(d => {
		  try {
		    i = i + 1
			d.tree.root = recLF(d.tree.root)
		  } catch {
		    case e : Exception => {
		      println("Failed to Left Factorize tree " + i)
		      
		    }
		  }
		})
	}
 
	abstract override def revert(data : List[ParseTreeData]) = {
		var i = -1
		//data.foreach(d => println(printTree(d.tree)))
		data.foreach(d => {
		  try {
		    i = i + 1
			d.tree.root = recUnLF(d.tree.root)
		  } catch {
		    case e : Exception => {
		      println("Failed to Reverse Left Factorize tree " + i)
		      //println(printTree(d.tree))
		      //  printStackTrace
		    }
		  }
		})
		//data.foreach(d => println("!" + printTree(d.tree)))
		super.revert(data)
	}
 
	def recUnLF(n : NonTerminalNode) : NonTerminalNode = {
  	  if(n.isInstanceOf[PreTerminalNode]) {
  		  return n
  	  }  
  	  n match {
  	    case BinaryTreeNode(s,left,right) => {
  	      if(isBarSymbol(right.symbol))
  	    	  accumFactors(s,List(left),right)
  	      else
  	    	  BinaryTreeNode(s,recUnLF(left),recUnLF(right))
        }
  	    case UnaryTreeNode(s,kid) => new UnaryTreeNode(s,recUnLF(kid))
  	  }
	}
 	
 	def accumFactors(sym : NonTerminal,
                   	 left : List[NonTerminalNode], 
                     right : NonTerminalNode) : NonTerminalNode = {
	  right match {
	    case UnaryTreeNode(s,kid) => {
	    	var kids = (kid :: left).reverse
	    	if(kids.length == 1) 
	    		UnaryTreeNode(sym,recUnLF(kids(0)))
	    	else if(kids.length == 2)
	    		BinaryTreeNode(sym,recUnLF(kids(0)),recUnLF(kids(1)))
	    	else
	    		ProtoTreeNode(sym,kids.map(recUnLF(_)))
	    }
	  	case BinaryTreeNode(s,aleft,aright) => {
	  		accumFactors(sym,aleft :: left,aright)
	  	}
	  }
 	}
	
 	def isBarSymbol(nt : NonTerminal) : Boolean = symbolStrings(nt.id).indexOf('@') >= 0
 
	def makeBarSymbol(base : NonTerminal) : NonTerminal = {
		var s = symbolStrings(base.id)
		if(s.indexOf('@') < 0) //not yet a bar
			s = "@" + s
	 	NonTerminal(addSymbol(s))
	}
 
 	def isLCSymbol(nt : NonTerminal) : Boolean = symbolStrings(nt.id).indexOf("lc") >= 0
 
	def recLF(n : NonTerminalNode) : NonTerminalNode = {
	  n match {
	  case pt : PreTerminalNode => pt
      case nt : NonTerminalNode => {
    	  val kids = nt.children.asInstanceOf[List[NonTerminalNode]] //already checked for PT
    	  val sym = nt.symbol
    	  if(kids.length == 1) {
    		  new UnaryTreeNode(sym,recLF(kids(0)))
    	  }
    	  else if (kids.length == 2) { //both children must be nonterminals
    		  if(isBarSymbol(kids(1).symbol) || !isLCSymbol(sym)) {
    			  new BinaryTreeNode(sym,recLF(kids(0)),recLF(kids(1)))
    		  } else {	
    			  val newRight = new UnaryTreeNode(makeBarSymbol(sym),recLF(kids(1)))
    			  new BinaryTreeNode(sym,recLF(kids(0)),newRight)
    		  }
    	  } else { //binarize
    	    var newSym = makeBarSymbol(sym)
    	    var left = recLF(kids(0))
    		val right = recLF(new ProtoTreeNode(newSym,kids.drop(1)))
    	  	new BinaryTreeNode(sym,left,right)
         }
      } 
    }
  }
 */
}
