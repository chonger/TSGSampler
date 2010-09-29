package enbuske.parse

import collection.mutable.HashMap
import io.Source
import java.io.StringReader

class PCFG() {
  
  var rules = new HashMap[ProtoRule,Double]()
  var lexiconRules = new HashMap[TerminalRule,Double]()

  var symbolStrings = List[String]()
  var symbolIDs = new HashMap[String,ParseTypes.Symbol]()
  var nextSymID : ParseTypes.Symbol = 0
  
  var terminalStrings = List[String]()
  var terminalIDs = HashMap[String,ParseTypes.Terminal]()
  var nextTermID : ParseTypes.Terminal = 0

  initSymTerm()

  //COPY CONSTRUCTOR
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
    
  def scoreTree(tree : ParseTree) : Double = {
    scoreFromNode(tree.root)
  }
  
  def scoreFromNode(n : NonTerminalNode) : Double = {
    n match {
      case ptn : PreTerminalNode => lexiconRules(ptn.rule)
      case un : UnderspecifiedNode => {
        if(un.assignment != null)
          scoreFromNode(un.assignment)
        else 
          1.0
      }
      case pn : ProtoNode => {
        //println(PCFGPrinter.ruleString(this,pn.rule))

        (rules(pn.rule) /: pn.children)((a,b) => a * scoreFromNode(b)) 
      }
    }
  }
  
  def size : Tuple2[Int,Int] = {(nextSymID,nextTermID)}

  //expects a file of one tree per line treebank format
  def read(filename : String) : List[ParseTree] = {
	//print("Reading " + filename + " for treebank format trees ... ")
	val filedata = Source.fromFile(filename).getLines
	val treestrs = (List[String]() /: filedata)((a,b) => if(b.charAt(0) != '(') (a(0) + b) :: a.drop(1) else b :: a)
	//println("Got " + treestrs.length + " trees")
	treestrs.reverse.map(s => growTree(s))
  } 
 
  //prints each tree on a single line in full text format
  def write(filename : String, data : List[ParseTree]) = {
    println("writing " + data.length + " trees to " + filename + " in treebank format")
    import java.io.{File,FileWriter,BufferedWriter}
	var bw = new BufferedWriter(new FileWriter(new File(filename)))

	data.foreach(d => {

      bw.write("\\s+".r.replaceAllIn(PCFGPrinter.treeToString(this,d)," ") + "\n")

      
    })
	bw.close
  }
}


/**
 * Estimate rules from a set of trees
 *
 */ 
class DirectlyEstimatedPCFG extends PCFG {  
  
  val nonterminalCounts = HashMap[ParseTypes.Symbol,Int]()
  val ruleCounts = HashMap[TreeRule,Int]()  

  def process(data : List[ParseTree]) = {
  	nonterminalCounts.clear
  	ruleCounts.clear
  	data.foreach(d => getCounts(d))
    initRules
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
