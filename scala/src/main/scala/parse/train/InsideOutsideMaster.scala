package parse.train

import collection.mutable._


class InsideOutsideMaster(data : List[ParseTree],val iterations : Int, val pcfg : PCFG) {
/**
  var logProb = 0.0
  val expectations = new HashMap[TreeRule,Double]()
  val expectationTotals = new HashMap[(ParseTypes.Symbol,ParseTypes.Split),Double]()
  
  var ioSlave = new InsideOutsideAlgorithm(data,pcfg)
  
  def getIOProbs(tree : ParseTree) = {} //ioSlave.getIOProbs(tree)
  
  def map() = {
	  ioSlave.run(Nil)
  }
  
  def reduce() = {
	 expectations.clear
	 expectationTotals.clear
	 logProb = 0.0
  ---
	 expectations ++= ioSlave.expectations
	 expectationTotals ++= ioSlave.expectationTotals
	 logProb += ioSlave.logProb
  ---
	 maximize()
  }
  
  def run(args : List[String]) = {  
	
	  //println("BEFORE EM")
	  //pcfg.printRules
	  
	  for{i <- 1 to iterations} {
	    
		  println("EM Iteration " + i + " beginning");
		 
		  val lastLogProb = logProb
		  map()
		  reduce()
		  
		  println("LOGPROB = " + logProb)
		  if(i > 2) {
			  val deltaLogProb = logProb - lastLogProb
			  if(deltaLogProb < 0)
				  throw new Exception("Increased log prob after EM : " + deltaLogProb)
			  println("Delta logProb = " + deltaLogProb)
		  }
	  }
		
	  //println("AFTER EM")
	  pcfg.printRules
  }
  
  def maximize() = {
	  var sum = 0.0
    
	  pcfg.rules.keys.foreach(r => {
		  var newval = 0.0
		  try  {
			  newval = expectations(r) / expectationTotals((r.lhs,r.split))
		  } catch {
		    //if a zeroprob tree hold the only instantiation of this rule, it didnt get expected
		    case e : NoSuchElementException => newval = pcfg.rules(r)
		  } 
		  
		  0000
		  if(r.lhs == 9) {
			  println(pcfg.ruleString(r) + " = " + (newval -  pcfg.rules(r)) +  " - " + pcfg.rules(r) + " to " + newval)
			  println(expectations(r))
			  println(expectationTotals((r.lhs,r.split)))
			  sum += newval
		  }
		  999
		  0000
		  if(r.isInstanceOf[UnaryRule] && r.asInstanceOf[UnaryRule].kid._1 == 1)
			  println(pcfg.ruleString(r) + " = " + (newval -  pcfg.rules(r)) +  " - " + pcfg.rules(r) + " to " + newval)
		0808000
    
          pcfg.rules += (r -> newval)
	  })
   
	  println("TOTAL FOR NN = " + sum)
	  //pcfg.printRules
  }
  */
}
