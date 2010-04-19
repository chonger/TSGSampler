package parse

import collection.mutable.HashMap
import io.Source
import java.io.StringReader

class PCFG() {

  def this(pcfg : PCFG) = {
    this()
    rules = pcfg.rules
    lexiconRules = pcfg.lexiconRules
    symbolStrings = pcfg.symbolStrings
    symbolIDs = pcfg.symbolIDs
    nextSymID = pcfg.nextSymID
  
    terminalStrings = pcfg.terminalStrings
    terminalIDs = pcfg.terminalIDs
    nextTermID = pcfg. nextTermID
  
    lock = pcfg.lock
  } 
  
  var rules = new HashMap[ProtoRule,Double]()
  var lexiconRules = new HashMap[TerminalRule,Double]()

  //NOTE : the string lists could be made into arrays, but if they are only used in writing, it doesnt really matter
  var symbolStrings = List[String]()
  var symbolIDs = new HashMap[String,ParseTypes.Symbol]()
  var nextSymID : ParseTypes.Symbol = 0
  
  var terminalStrings = List[String]()
  var terminalIDs = HashMap[String,ParseTypes.Terminal]()
  var nextTermID : ParseTypes.Terminal = 0
    

  def getSym(nt : NonTerminalNode) : String = {
    symbolStrings(nt.symbol)
  }

  def getTerm(tn : TerminalNode) : String = {
    terminalStrings(tn.terminal)
  }

  var lock = false
  def setLock(b : Boolean) = {
    lock = b
  }
  
  initSymTerm()
  
  override def equals(any : Any) : Boolean = {
    any match {
      case pcfg : PCFG => {
        rules.foreach(r => {
        	if(pcfg.rules.getOrElse(r._1,0.0) != r._2)
        		return false
        })
        lexiconRules.foreach(r => {
        	if(pcfg.lexiconRules.getOrElse(r._1,0.0) != r._2)
        		return false
        })
        symbolStrings.zip(pcfg.symbolStrings).foreach(_ match {
          case (a,b) => if (a != b) return false 
        })
        terminalStrings.zip(pcfg.terminalStrings).foreach(_ match {
          case (a,b) => if (a != b) return false 
        })
        true
      }
      case _ => false
    }
  }
  
  def clear = {rules.clear ; lexiconRules.clear} //clear rule data, but not symbols/terminals
  
  /**
   * this makes sure that shortcut types get put in at the right index
   */
  def initSymTerm() = {
	  symbolStrings = List[String](ParseTypes.RootString)
	  symbolIDs.clear
	  symbolIDs += (ParseTypes.RootString -> ParseTypes.Root)
	  terminalStrings = List[String](ParseTypes.EmptyString)
	  terminalIDs.clear
	  terminalIDs += (ParseTypes.EmptyString -> ParseTypes.Empty)
	  nextSymID  = ParseTypes.Root
	  nextTermID  = ParseTypes.Empty
  }
 
  def addSymbol(s : String) = symbolIDs.getOrElseUpdate(s,{
	  //println("adding symbol " + s)
	  if(lock)
		  throw new Exception("PCFG is locked, trying to add " + s)
	  symbolStrings =  (symbolStrings.reverse.::(s)).reverse //TODO this sucks
	  nextSymID = ParseTypes.intToSymbol(nextSymID.toInt + 1)
	  nextSymID
  	})
  
  def addTerm(s : String) : ParseTypes.Terminal = terminalIDs.getOrElseUpdate(s,{
	  //println("adding terminal " + s + " " + (nextTermID + 1))
	  if(lock)
		  throw new Exception("PCFG is locked, trying to add" + s)
	  terminalStrings =  (terminalStrings.reverse.::(s)).reverse //TODO see above note of suckage
	  nextTermID += 1
	  nextTermID
  	})
  
    
  def growTree(s : String) : ParseTree = {
	  
	def isWhitespace(c : Char) = List(' ','\t','\r','\n') contains c
	def readNode(stream : StringReader) : NonTerminalNode = {
		var ntStr = new StringBuffer()
		var c = stream.read.toChar
		while(!isWhitespace(c)) {ntStr append c; c = stream.read.toChar}
		while(isWhitespace(c)) {c = stream.read.toChar} 
		var kids = List[NonTerminalNode]()
		var sym = addSymbol(ntStr.toString)
		if(ntStr.toString == "ROOT") {
			sym = ParseTypes.Root
		}
		while(stream.ready) {
			c match {
				case '(' => kids ::= readNode(stream)
				case ')' => return ProtoNode(sym,kids.reverse)
				case _ if isWhitespace(c) => {}
				case _ => {
					var termStr = new StringBuffer()
					while(c != ')') {
						termStr append c
						c = stream.read.toChar
					}  
					if(termStr.toString == "<>") {
						return UnderspecifiedNode(sym,null)
					}
					else
						return PreTerminalNode(sym,
							TerminalNode(addTerm(termStr.toString)))
				}	
			}
			c = stream.read.toChar
		}
		  null
	}
	var stringstream = new StringReader(s)
	stringstream.read 
	new ParseTree(readNode(stringstream))
  }
  
  def klDiv(apcfg : PCFG) : Double = {
    println("MY PCFG")
    println(("" /: PCFGPrinter.rulesToString(this))(_ + _ + "\n"))
    println("APCFG")
    println(("" /: PCFGPrinter.rulesToString(apcfg))(_ + _ + "\n"))
    def klDivSumF[A,B <: Double](x : Tuple2[A,B], other : HashMap[A,B]) : Double = {
      x match {
        case (r,d) => {
	      val myProb = d
	      //println(PCFGPrinter.ruleString(this,r.asInstanceOf[TreeRule]))
	      val otherProb = other.getOrElse(r,throw new Exception())
	      myProb * (Math.log(myProb) - Math.log(otherProb))
        }
      }
    }
    val rSum = rules.map(klDivSumF(_,apcfg.rules)) 
    val lSum = lexiconRules.map(klDivSumF(_,apcfg.lexiconRules))
    println(rSum.toList)
    println(lSum.toList)
    (0.0 /: rSum)(_ + _) + (0.0 /: lSum)(_ + _)
  }
  
  def scoreTree(tree : ParseTree) : Double = scoreFromNode(tree.root)
  def scoreFromNode(n : NonTerminalNode) : Double = {
    n match {
      case ptn : PreTerminalNode => lexiconRules(ptn.rule)
      case un : UnderspecifiedNode => {
        if(un.assignment != null)
          scoreFromNode(un.assignment)
        else 
          1.0
      }
      case pn : ProtoNode => (rules(pn.rule) /: pn.children)((a,b) => a * scoreFromNode(b)) 
    }
  }
  
  def size() : Tuple2[Int,Int] = (nextSymID,nextTermID)
}

class FileBasedPCFG extends PCFG {
	/**
	def read(filename : String) = {
	  readSplits(filename + ".splits")
	  readGrammar(filename + ".grammar")
	  readLexicon(filename + ".lexicon")
	}
	
	def readSplits(filename : String) = {	
		val splitstrs = Source.fromFile(filename).getLines.toList //toList is needed to know length
  
		//use an ArrayBuffer based object before setting the final split tree
		import scala.collection.mutable.ArrayBuffer
		//plus one b/c root is not in the splits file and is never split
		val readerTree = new Array[ArrayBuffer[ArrayBuffer[Tuple2[ParseTypes.Split,Int]]]](splitstrs.length + 1)
    
		readerTree(ParseTypes.Root) = new ArrayBuffer[ArrayBuffer[Tuple2[ParseTypes.Split,Int]]]()
		readerTree(ParseTypes.Root) += new ArrayBuffer[Tuple2[ParseTypes.Split,Int]]() 
  
		splitstrs.map(_.split("\t")).foreach(spl => {
			val sym = addSymbol(spl(0))
			readerTree(sym) = new ArrayBuffer[ArrayBuffer[Tuple2[ParseTypes.Split,Int]]]()

			val stream = new StringReader(spl(1))
			stream.read
 
			def readSplitTreeNode(pindex : Int, depth : Int) : Unit = {			
				if(readerTree(sym).size <= depth)
					readerTree(sym) += (new ArrayBuffer[Tuple2[ParseTypes.Split,Int]]())
    
			    var c = stream.read.toChar
			    var index = readerTree(sym)(depth).size
			    var idStr = ""
			    var idList = List[ParseTypes.Split]() 
       
				while(stream.ready) {
					c match {
						case '(' => readSplitTreeNode(index, depth + 1)
						case ')' => {
						  if(idStr.length != 0) {
							  idList ++= List(ParseTypes.parseSplit(idStr))
							  idStr = ""
						  }
						  
						  readerTree(sym.id)(depth) += (idList(0),pindex)     
						  
						  splitTrees = readerTree.flatMap(a => {
							  if(a == null)
								  Nil
							  else		
								  List(a.toList.map(b => b.toList))
						  })
						  idList.drop(1).foreach(e => {
							  if(readerTree(sym.id).size <= depth + 1)
								  readerTree(sym.id) += (new ArrayBuffer[Tuple2[ParseTypes.Split,Int]]())
							  readerTree(sym.id)(depth + 1) += (e,index)
						  })
						  return
						}
						case _ if (c == ' ') => {
							if(idStr.length != 0) {
								idList ++= List(ParseTypes.parseSplit(idStr))
								idStr = ""
							}
						}
						case _ => {
							idStr += c
						}
					}
					c = stream.read.toChar
				}
			}
			readSplitTreeNode(0,0)
		})
		//fill in the first tree with an unsplit root
		
		splitTrees = readerTree.map(a => a.toList.map(b => b.toList))
		maxSplit = splitTrees(1).size - 1
		splitTrees(ParseTypes.Root) = (for{i <- 0 to maxSplit} yield {List((ParseTypes.Unsplit,0))}).toList
		
	}
	*/
 
	/**
	def readGrammar(filename : String) = {
		def getSplitNT(s : String) = {
			var parts = s.split("\\^g_")
			if(parts.length == 1) //it seems that if a symbol is never split, ^g is not used
				parts = s.split("_")
			SplitNT(addSymbol(parts(0)),ParseTypes.parseSplit(parts(1)))
		}
	  
		val frules = Source.fromFile(filename).getLines
		frules.map(_.split("\\s")).foreach(r => {
			val len = r.length
			val lhs = getSplitNT(r(0))
			val rhs = r.slice(2,len - 1).map(a => {val b = getSplitNT(a) ; (b.id,b.split)})	
			val prob = r(len - 1).toDouble
			if(rhs.size == 2)
				rules += (BinaryRule(lhs.id,lhs.split,rhs(0),rhs(1)) -> prob)
			else {
				if(!((lhs.id == rhs(0)._1) && (lhs.split == rhs(0)._2) && (prob == 1.0)))
					rules += (UnaryRule(lhs.id,lhs.split,rhs(0)) -> prob)
			}
		})
	}
 
	def readLexicon(filename : String) = {
	  def tidy(s : String) = "[,\\[\\]]".r.replaceAllIn(s,"")
	  val frules = Source.fromFile(filename).getLines
	  frules.map(_.split("\\s")).foreach(r => {
		  val lhs = addSymbol(r(0))
		  val rhs = addTerm(r(1))

		  val tidied = r.drop(2).map(tidy(_)).toList
		  tidied.zipWithIndex.foreach(v => {
			  lexiconRules += (TerminalRule(lhs,ParseTypes.parseSplit(v._2.toString),rhs) -> v._1.toDouble)
		  })
	  })
	}  
	*/
}

//Estimate rules from a set of trees.  Assumes unsplit nodes
class DirectlyEstimatedPCFG extends PCFG {  
  
  val nonterminalCounts = HashMap[ParseTypes.Symbol,Int]()
  val ruleCounts = HashMap[TreeRule,Int]()
  
  def process(data : List[ParseTree]) = {
  	nonterminalCounts.clear
  	ruleCounts.clear
  	data.foreach(d => getCounts(d))
    initRules
    //rules.foreach(_ match {case (r,d) => println(PCFGPrinter.ruleString(this,r) + " = " + d)})
  }
  
  override def clear = {
    super.clear
  	nonterminalCounts.clear
  	ruleCounts.clear
  }
  
  def getCounts(tree : ParseTree) = {
	  recGetCounts(tree.root)
  }	
  
  def recGetCounts(n : NonTerminalNode) : Unit = {
  	var ntCount = nonterminalCounts.getOrElse(n.symbol,0) + 1
  	nonterminalCounts += (n.symbol -> ntCount)
    n match {
	  case pt : PreTerminalNode => { 
		  val rule = new TerminalRule(pt.symbol,pt.kid.terminal)
		  var rCount = ruleCounts.getOrElse(rule,0) + 1
		  ruleCounts += (rule -> rCount)
          	  
  	  }
	  case nt : ProtoNode => {
		  val rule = nt.rule
  	   	  var rCount = ruleCounts.getOrElse(rule,0) + 1
  	   	  ruleCounts += (rule -> rCount)
          //recursively continue committing data
  	   	  nt.children.foreach(recGetCounts(_))
  	   	}
	  }
  }
  
  //used when all trees are loaded in to estimate pcfg probabilities
  def initRules() = {
	  rules.clear
	  lexiconRules.clear
	  ruleCounts.foreach(_ match {
	    case (rule,d) => {
	    	val ntCount = nonterminalCounts(rule.lhs).toDouble
	    	rule match {	
	        	case tr : TerminalRule => lexiconRules += (tr -> (d / ntCount))
	        	case pr : ProtoRule => rules += (pr -> d / ntCount)
	    	}
	    }
	})     
  } 
}
