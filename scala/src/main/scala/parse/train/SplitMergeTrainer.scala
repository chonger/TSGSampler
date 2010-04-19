package parse.train

class SplitMergeTrainer(data : List[ParseTree], 
                        val pcfg : PCFG,
                        val smIterations : Int,
                        val emIterations : Int) {
/**  
  val ioAlg = new InsideOutsideMaster(data,emIterations,pcfg)
  
  val mergeThreshold = -1e4
  
  def map() = {}
  def reduce() = {}
  
  def run(args : List[String]) = {       
	for{i <- 1 to smIterations} {
		ioAlg.run(Nil)
		//splitSymbols
		//splitRules()
		//merge()
	}
  }  
  
  def getMergeList() = {
    88
    NonTerminal.splits.map(sl => {
    	for{s1 <- sl
    		s2 <- sl
    		if ParseTypes.isPair(s1,s2)}
    	yield {(s1,s2)}
    })
    88                    
  }
  88
  def merge() = {
    println("MERGING")
    val delta = new HashMap[(ParseTypes.Symbol,ParseTypes.Split,ParseTypes.Split),Double]()
    var toMerge : Array[List[(ParseTypes.Split,ParseTypes.Split)]] = getMergeList
    
    def getDelta(tree: ParseTree) = {
    	val (insideProbs,outsideProbs) = ioAlg.getIOProbs(tree)
    	tree.nodes.foreach(n => n.symbol match {
    	  case r : Root => {/* do nothing */}
    	  case t : Terminal => {/*do nothing*/}
    	  case NonTerminal(symbol) => {
    	    val splitProbs = NonTerminal.splits(symbol).map(s => insideProbs(n,s) * outsideProbs(n,s))
    	    val total = (0.0 /: splitProbs)(_ + _)
    	    for {(s1,s2) <- toMerge(symbol)} {
    	    	var accum = 0.0
    	    	for{(os,ind) <- NonTerminal.splits(symbol).zipWithIndex
    	    		if(os != s1 && os != s2)} {accum += splitProbs(ind)}
    	    	var newIn = outsideProbs(n,s1) + outsideProbs(n,s2)
    	    	val p1 = ioAlg.expectationTotals((symbol,s1))
    	    	val p2 = ioAlg.expectationTotals((symbol,s2))
    	    	newIn *= (p1 * insideProbs(n,s1) + p2 * insideProbs(n,s2))
    	    	delta((symbol,s1,s2)) = Math.log(newIn) - Math.log(accum)
    	    }
    	  }
    	})
    }
    
    data.foreach(d => getDelta(d.tree))
    
    NonTerminal.splits = NonTerminal.splits.map(a => List(ParseTypes.Unsplit))
    for{((sym,s1,s2),del) <- delta} {
    	println("DEL" + del)
    	if(del < mergeThreshold) {
    	  println("Merging " + pcfg.symbolStrings(sym) + " " + s1.toInt + "," + s2.toInt)
    	  NonTerminal.splits(sym) ::= ParseTypes.merge(s1)	
    	} else 
    	  NonTerminal.splits(sym) :::= List(s1,s2)
    }
  }
  
  def splitSymbols() = {
    NonTerminal.splits = (NonTerminal.splits.first :: 
                            NonTerminal.splits.toList.drop(1).map(s =>
                            	s.flatMap(ParseTypes.split(_))
                            )).toArray  
  }
  
  def splitRuleSym(sym : ParseTypes.Symbol,split : ParseTypes.Split) : List[ParseTypes.Split] = {
	  if(sym == ParseTypes.Root)
		  List(split)
	  else
		  ParseTypes.split(split)
  }
  
  def splitRules() = {
    var newRules = pcfg.rules.flatMap(e => {
      var rule = e._1
      var oVal = e._2
      rule match {
        case TerminalRule(symbol,split,term) => {
          for{psplit <- splitRuleSym(symbol,rule.split)}
          yield (new TerminalRule(symbol,psplit,term) -> oVal * randomNoise)
        }
        case UnaryRule(lhs,split,kid) => {
          for{psplit <- splitRuleSym(lhs,rule.split)
              ksplit <- ParseTypes.split(kid._2)} 
          yield (new UnaryRule(rule.lhs,psplit,(kid._1,ksplit)) -> oVal * randomNoise)
        }
        case BinaryRule(lhs,split,left,right) => {
          for{psplit <- splitRuleSym(lhs,rule.split)
              lsplit <- ParseTypes.split(left._2)
          	  rsplit <- ParseTypes.split(right._2)} 
          yield (new BinaryRule(rule.lhs,psplit,(left._1,lsplit),(right._1,rsplit)) -> oVal * randomNoise)
        }
      }
    })
      
    pcfg.rules.clear
    newRules.foreach(pcfg.rules += _)
  }
  88
  val rand = new java.util.Random(12345)
  //random number between _+.02
  def randomNoise() = (rand.nextDouble * 4.0 - 2.0) / 100.0 + 1.0
  */
}

