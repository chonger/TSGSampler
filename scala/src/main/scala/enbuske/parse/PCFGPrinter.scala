package enbuske.parse

object PCFGPrinter {
  def rulesToString(pcfg : PCFG) = {
    pcfg.rules.map(r => ruleString(pcfg,r._1) + " = " + r._2)
  }
  def lexRulesToString(pcfg : PCFG) = {
    pcfg.lexiconRules.map(r => ruleString(pcfg,r._1) + " = " + r._2)
  }

  def ruleString(pcfg : PCFG, r : TreeRule) = {
	  pcfg.symbolStrings(r.lhs) + " --> " + {
	    r match {
	  		case pr : ProtoRule => ("" /: pr.rhs)((a,b) => a + " " + pcfg.symbolStrings(b))
	  		case tr : TerminalRule => pcfg.terminalStrings(tr.terminal)
    }}
  }
  
  def nodeString(pcfg : PCFG, n : TreeNode) : String = { 
    n match {
      case un : UnderspecifiedNode => {
        val kidStr = if(un.assignment == null) "" else " -> " + pcfg.symbolStrings(un.assignment.symbol)
        "*" + pcfg.symbolStrings(un.symbol) + "*" + kidStr
      }
      case nt : InternalNode => {
        ((pcfg.symbolStrings(nt.symbol) + " -> ") /: (nt.children))((a,b) => 
          a + " " + pcfg.symbolStrings(b.symbol))
      }
      case PreTerminalNode(sym,kid) => {
        pcfg.symbolStrings(sym) + " -> " + pcfg.terminalStrings(kid.terminal)
      }
      case TerminalNode(term) => pcfg.terminalStrings(term)
    }
  }
  
  def getSpcStr(n : Int) = {
    var ret = ""
    for(i <- 1 to n)
      ret += " "
    ret
  }

  def treeToString(pcfg : PCFG, tree : ParseTree) = {
	recTreeToString(pcfg,tree.root,"")
  }
  
  
  def recTreeToString(pcfg : PCFG, n : TreeNode, offset : String) : String = {
    n match {
      case un : UnderspecifiedNode => {
        var symstr = pcfg.symbolStrings(un.symbol)
        var ret = "(*" + symstr + " "
        if(un.assignment != null) {
        	var spcstr = getSpcStr(symstr.length + 2)
        	ret += recTreeToString(pcfg,un.assignment,offset)
        }
        ret + "*)"
      }
      case nt : NonTerminalNode =>
        //var symstr = (symbolStrings(nt.symbol.id) /: nt.symbol.splits)(_ + "/" + _.toInt.toString)
        
        var symstr = pcfg.symbolStrings(nt.symbol)
    	var ret = "(" + symstr + " "
    	var spcstr = getSpcStr(symstr.length + 2)
    
    	ret += (recTreeToString(pcfg,nt.children(0),offset + spcstr) /: nt.children.drop(1))((a,b) => {
    	    a + "\n" + offset + spcstr + recTreeToString(pcfg,b,offset + spcstr)})
    	ret + ")"
      case TerminalNode(terminal) => {
        try{
          pcfg.terminalStrings(terminal)  
        } catch {
          case _ => {println("TERM! " + terminal) 
                     throw new Exception()}
        }
      }
    }
  }  
}
