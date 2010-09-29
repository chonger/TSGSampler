package enbuske.util

import parse._

class Unker {
  //override these two
  def unk_?(pn : PreTerminalNode) : Boolean = true
  def unkToken(pt : ParseTypes.Symbol, s : String, pos : Int) : String = "EPSILON"

  /**
  def unkSentence(sent : List[String]) : List[String] = {
    sent.zipWithIndex.map(_ match {case (s,i) => if(unk_?(s)) unkToken(s,i) else s})
  }
  */
  def unkData(data : List[ParseTree], pcfg : PCFG) : Tuple2[List[ParseTree],PCFG] = {
    
    val unkPCFG = new PCFG(pcfg)
    
    val trees = data.map(t => {
      
      val terminalOrder = t.terminals
      
      def findIndex(tn : TerminalNode) : Int = {
    	  for{i <- 0 to terminalOrder.length - 1} {
    	    if(tn eq terminalOrder(i))
    	    	return i
    	  }
    	  throw new Exception("Not Found")
      } 
      
      def unkNode(nt : NonTerminalNode) : NonTerminalNode = {
	      nt match {
		    case pn : PreTerminalNode => {
		      if(unk_?(pn)) {
		    	  val index = findIndex(pn.kid)
		    	  val unkedS = unkPCFG.addTerm(unkToken(pn.symbol,pcfg.terminalStrings(pn.kid.terminal),index))
		    	  PreTerminalNode(pn.symbol,TerminalNode(unkedS))
		      } else
	        	  PreTerminalNode(pn.symbol,TerminalNode(pn.kid.terminal))
		    }
		    case pn : ProtoNode => {
              val str = pcfg.symbolStrings(pn.symbol)
              val eInd = str.indexOf('=')
              val newStr = if(eInd >= 0) {
                str.substring(0,eInd)
              } else {
                str
              }
              ProtoNode(unkPCFG.addSymbol(newStr),pn.children.map(unkNode(_)))

            }
		    case un : UnderspecifiedNode => {
		      if(un.assignment != null) 
		    	  UnderspecifiedNode(un.symbol,unkNode(un.assignment))
		      else
		    	  UnderspecifiedNode(un.symbol,null)
	        }                             
		  }
      }
       
      new ParseTree(unkNode(t.root))
    })
    
    (trees,unkPCFG)
  }
}

class UnkFromData(data : List[ParseTree], pcfg : PCFG) extends UnkLeastCommon(0,Nil,pcfg) {

  override def initMe() {
    data.foreach(t => {
      lexicon ++= t.nonterminals.filter(_.isInstanceOf[PreTerminalNode]).map(_.asInstanceOf[PreTerminalNode])
    })
  }

}


//Unks any terminal which occurs at most n times
class UnkLeastCommon(n : Int, data : List[ParseTree], pcfg : PCFG) extends Unker {

	import scala.collection.mutable.HashMap
	import scala.collection.mutable.HashSet

    val lexicon = new HashSet[PreTerminalNode]()
    initMe()

    def initMe() = {
	  println("Finding a lexicon of words which occur more than " + n + " times")	  
 
	  val counts = new HashMap[PreTerminalNode,Int]()
	  data.foreach(t => t.nonterminals.foreach(n => {
        n match {
          case pn : PreTerminalNode => 
		    counts(pn) = counts.getOrElse(pn,0) + 1
          case _ => {/**nothing*/}
        }
	  }))
	  counts.keySet.foreach(k => {
		if(counts(k) > n)
			lexicon += k
	  })
 
	  println("Got a lexicon of size " + lexicon.size)
    }


	override def unk_?(s : PreTerminalNode) = !(lexicon contains s)
 
	/**
	 * Uses the fifth level of the berkely parser's unker
	 */
	override def unkToken(pt : ParseTypes.Symbol, s : String, pos : Int) : String = {
	  
		def myUpper(c : Char) = c.isUpperCase || Character.isTitleCase(c)
	  
		val sb = new StringBuffer("UNK")
	  
		var wLen = s.length
		if(wLen == 0)
			throw new Exception
		var numCaps = s.filter(myUpper(_)).length
		var hasDigit = !s.filter(_.isDigit).isEmpty 
		var hasDash = !s.filter(_ == '-').isEmpty
		var hasLower = wLen != numCaps
  
		var firstChar = s(0)
		var lowered = s.toLowerCase
		if(myUpper(firstChar)) {
		  if(pos == 0 && numCaps == 1) {
		    sb.append("-INITC")
            
		    if(pcfg.terminalIDs.isDefinedAt(lowered) && 
               !unk_?(PreTerminalNode(pt,TerminalNode(pcfg.terminalIDs(lowered))))) {//we know it lowercased
                 sb.append("-KNOWNLC")
		    }

		  } else sb.append("-CAPS")
		} else if (!Character.isLetter(firstChar) && numCaps > 0) {
		  sb.append("-CAPS")
		} else if(hasLower) sb.append("-LC")
  
		if(hasDigit) sb.append("-NUM")
		if(hasDash) sb.append("-DASH")
		
		if(lowered.endsWith("s") && wLen >3) {
		  if(!(List('s','i','u') contains lowered(wLen - 2))) sb.append("-s")
		} else if(wLen >= 5 && !hasDash && !(hasDigit && numCaps > 0)) {
		  
		  val suffixes = List("ed","ing","ion","er","est","ly","ity","y","al")
		  def getSuffix() : String = {
		    for{s <- suffixes}{if(lowered.endsWith(s)) return s}
		    return null
		  }
	
		  val suffix : String= getSuffix()
		  if(suffix != null)
			  sb.append("-" + suffix)
		}
  
		sb.toString
	}
}
