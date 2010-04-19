package parse.train


import collection.mutable.HashMap
import parse._

class InsideOutsideAlgorithm(data : List[ParseTree], val pcfg : PCFG) {
 /**
	//This is fully mapped
	def reduce() = {}
	def map() = {}
  
	var logProb = 0.0
	val expectations = new HashMap[TreeRule,Double]()
	val expectationTotals = new HashMap[(ParseTypes.Symbol,ParseTypes.Split),Double]()
 
	def run(args : List[String]) = {
	  expectations.clear
	  expectationTotals.clear
	  logProb = 0.0
	  data.map(d => processTree(d.tree))
	}
 
	def processTree(tree : ParseTree) : Unit = {
		
		val (insideProbs,outsideProbs) = getIOProbs(tree)

		00000
		//println("INSIDE")
		//insideProbs.filter(_._1._1.symbol == 1).foreach(k => println("IN" + pcfg.nodeString(k._1._1) + "-" + k._1._2.toInt + " -- " + k._2))
		//insideProbs.foreach(k => println("" + pcfg.nodeString(k._1._1) + "-" + k._1._2.toInt + " -- " + k._2))
		//println("OUTSIDE")
		//outsideProbs.filter(a => pcfg.symbolStrings(a._1._1.symbol.asInstanceOf[NonTerminal].id) == "NNP").foreach(k => println("" + pcfg.nodeString(k._1._1) + "-" + k._1._2.toInt + " -- " + k._2))
		//outsideProbs.foreach(k => println("" + pcfg.nodeString(k._1._1) + "-" + k._1._2.toInt + " -- " + k._2))
		00000000
  
		def getTotalProb(nt : NonTerminalNode) : Double = {
			(0.0 /: nt.symbol.splits(pcfg))((a,b) => a + insideProbs((nt,b)) * outsideProbs((nt,b)))
		}
  
		var totalProb = getTotalProb(tree.root)
  
		if("debug" == "debug") {
			if(totalProb == 0) {
			  00000
			  println("PROB = 0 ")
			  println(pcfg.printTree(tree))
			  println("INSIDE")
			  insideProbs.filter(_._1._1.symbol == 1).foreach(k => println("IN" + pcfg.nodeString(k._1._1) + "-" + k._1._2.toInt + " -- " + k._2))
			  insideProbs.foreach(k => println("" + pcfg.nodeString(k._1._1) + "-" + k._1._2.toInt + " -- " + k._2))
			  println("OUTSIDE")
			  outsideProbs.filter(a => pcfg.symbolStrings(a._1._1.symbol.asInstanceOf[NonTerminal].id) == "NNP").foreach(k => println("" + pcfg.nodeString(k._1._1) + "-" + k._1._2.toInt + " -- " + k._2))
			  outsideProbs.foreach(k => println("" + pcfg.nodeString(k._1._1) + "-" + k._1._2.toInt + " -- " + k._2))
			  throw new Exception("ZEROPROB")
			  00000000000
			  println("ZEROPROB")
			  return
			}
			def check(tree : ParseTree) = {
				var ok = true
				tree.nonterminals.foreach(n => {
					if(Math.abs(getTotalProb(n) - totalProb) > 1e-16) {
						println("BAD CHECK " + pcfg.nodeString(n) + " -> " + getTotalProb(n) + "/" + totalProb + " -- " + (totalProb - getTotalProb(n)))
						ok = false
					}
				})
				ok
			}
			check(tree)
		}
  
		logProb += Math.log(totalProb)
  
		//aggregate the expectations for rules over all nodes in the tree
		var expects = tree.nodes.flatMap(n => 
		  	n match {
		  	  case t : TerminalNode => Nil
		  	  case p : PreTerminalNode => {
		  		  for{rule <- p.rules(pcfg)} yield {
		  		    //println(" P " + pcfg.nodeString(n) + " - " + rule.split.toInt + " --> " + pcfg.rules(rule) * outsideProbs((n,rule.split)))
		  		    (rule,pcfg.lexiconRules.getOrElse(rule,dumbSmooth) * outsideProbs((n,rule.split)))
		  		  }

		  	  }
		  	  case u : UnaryTreeNode => {
		  		  //println(" U " + pcfg.nodeString(n))
		  		  for{rule <- u.rules(pcfg)} yield 
		  		  (rule,pcfg.rules.getOrElse(rule,dumbSmooth) * outsideProbs((n,rule.split)) * insideProbs((u.kid,rule.kid._2)))
		  	  }
		  	  case b : BinaryTreeNode => {
		  		  //println(" B " + pcfg.nodeString(n))
		  		  for{rule <- b.rules(pcfg)} yield
		  		  (rule,pcfg.rules.getOrElse(rule,dumbSmooth) * outsideProbs(n,rule.split) * 
		  				  insideProbs((b.left,rule.left._2)) * insideProbs((b.right,rule.right._2))) 
		  	  }
		  	})
		
		//for each rule expectation normalize and record for each rule and each lhs  	
	  	expects.foreach(e => {
	  		000
	  		if(e._1.lhs == 9) {
	  			println("\tUPDATE "  + pcfg.ruleString(e._1) + " - " + e._2 )
	  			println("\tTOTALP = " + totalProb)
	  			println("\tRULEP = "  + pcfg.rules(e._1))
	  			outsideProbs.filter(a => pcpfg.symbolStrings(a._1._1.symbol.asInstanceOf[NonTerminal].id) == "NN").foreach(k => println("" + pcfg.nodeString(k._1._1) + "-" + k._1._2.toInt + " -- " + k._2))
	  		}
            0000000000
	  	  	addToMap(expectations,e._1,e._2 / totalProb)
	  		addToMap(expectationTotals,(e._1.lhs,e._1.split),e._2 / totalProb)
	  	}) 
	}
 
	//utility for updating a map of A -> double which handles a miss
	def addToMap[A](map : HashMap[A,Double], key : A, update : Double) = {
	  val upd = map.getOrElse(key,0.0) + update
	  map += (key -> upd)
	}
 
	type IOType = HashMap[(TreeNode,ParseTypes.Split),Double]

	val dumbSmooth = 1e-12
 
	def getIOProbs(tree : ParseTree) = {
		val insideProbs = new IOType()
		val outsideProbs = new IOType()
  
		def getInside(n : NonTerminalNode) : Unit = {
			n match {
			  case ptn : PreTerminalNode => ptn.rules(pcfg).foreach(rule => {
				  var inprob = pcfg.lexiconRules.getOrElse(rule,dumbSmooth)
				  addToMap(insideProbs,(ptn,rule.split),inprob)
			  })  
			  case un : UnaryTreeNode => {
			    getInside(un.kid)
			    
			    un.rules(pcfg).foreach(rule => {
			    	var inprob = pcfg.rules.getOrElse(rule,dumbSmooth  )
			    	inprob *= insideProbs((un.kid,rule.kid._2))
			    	addToMap(insideProbs,(un,rule.split),inprob)
			    })
			    
			  }
			  case bn : BinaryTreeNode => {
				  getInside(bn.left)
				  getInside(bn.right)
				  bn.rules(pcfg).foreach(rule => {
			    	var inprob = pcfg.rules.getOrElse(rule,dumbSmooth) * 
			    				insideProbs((bn.left,rule.left._2)) * 
			    				insideProbs((bn.right,rule.right._2))
			    	addToMap(insideProbs,(bn,rule.split),inprob)
				  })
			  }
			}
		}
	
		def getOutside(n : NonTerminalNode) : Unit = {
			n match {
			  case ptn : PreTerminalNode => {/*do nothing*/} 
			  case un : UnaryTreeNode => {
			    un.rules(pcfg).map(rule => {
			    	var update = outsideProbs((un,rule.split)) * pcfg.rules.getOrElse(rule,dumbSmooth)
				    addToMap(outsideProbs,(un.kid,rule.kid._2),update)
			    })
			    getOutside(un.kid)
			  }
			  case bn : BinaryTreeNode => {
				  bn.rules(pcfg).map(rule => {
					  var update = outsideProbs((bn,rule.split)) * pcfg.rules.getOrElse(rule,dumbSmooth)
					  addToMap(outsideProbs,(bn.left,rule.left._2),update * insideProbs(bn.right,rule.right._2))
					  addToMap(outsideProbs,(bn.right,rule.right._2),update * insideProbs(bn.left,rule.left._2))
				  })
				  getOutside(bn.left)
				  getOutside(bn.right)
			  }
			}		
		}
  
		getInside(tree.root)
  
		//println("Inside probs")
		//insideProbs.foreach(p => println(pcfg.nodeString(p._1._1) + "/" + p._1._2.toInt + " --- " + p._2))
  
		outsideProbs += ((tree.root,ParseTypes.Unsplit) -> 1.0)
		getOutside(tree.root.asInstanceOf[NonTerminalNode])
	
		(insideProbs,outsideProbs)
	} 
  */
}
