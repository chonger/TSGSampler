package parse.tsg.train

trait ConcentrationParameterResampling extends TSGTrainer {

	/**
	 *
	 *              PARAMETER RESAMPLING
	 * 
	 */
  
	var occurences : Array[Int] = null
  
	override def initFromData() : Int = {
		val max = super.initFromData()
		occurences = new Array[Int](max + 1)
		data.foreach(t => t.nonterminals.foreach(nt => {
			occurences(nt.symbol) = occurences(nt.symbol) + 1
		}))
		
		max
	}
 
	def sampleAlpha = ntset(alphas,(nt : NonTerm) => resampleAlpha(nt))
	def sampleBeta = ntset(betas,(nt : NonTerm) => resampleBeta(nt))
	val ALPHA_SIGSQ = 20
	val GAMMA_A = 1
	val GAMMA_B= 5	

	def getNumClasses(nt : NonTerm) = {
	  (0 /: counts.keys)((accum,tree) => accum + {
	    if(tree.root.symbol == nt && counts(tree)._1 > 0) {
	    	1 
	  	} else 
	    	0
	  })
	}
 
	//def resampleAlpha = resampleAlphaEscobarWest _
	def resampleAlpha = resampleAlphaMetropolisH _	  
 
	def resampleAlphaEscobarWest(nt : NonTerm) : Double = {
		val k = getNumClasses(nt)
		val n = occurences(nt)
		println(n + " instances and " + k + " classes for " + ntStr(nt))
		val ret = sampling.DPAlpha.resampleAlphaEscobarWest(k,n,alphas(nt),GAMMA_A,GAMMA_B)
		println("ALPHA RESAMPLE FOR " + ntStr(nt) + " - " + alphas(nt) + " -> " + ret)
		ret
	}  
	def resampleAlphaMetropolisH(nt : NonTerm) = {
		val k = getNumClasses(nt)
		val n = totals(nt)
		//println(n + " instances and " + k + " classes for " + ntStr(nt))
		val ret = sampling.DPAlpha.resampleAlphaMetropolisH(k,n,alphas(nt),GAMMA_A,GAMMA_B,ALPHA_SIGSQ)
		//println("ALPHA RESAMPLE FOR " + ntStr(nt) + " - " + alphas(nt) + " -> " + ret)
		ret
	}
	def resampleBeta(nt : NonTerm) : Double = {
		var expandedCount = totals(nt)
		var nonExpCount = occurences(nt) - expandedCount
		//println("NonExpanded = " + nonExpCount + ", Expanded = " + expandedCount)
		val ret = rando.nextBeta(1 + expandedCount,1 + nonExpCount)
		//println("BETA RESAMPLE FOR " + ntStr(nt) + " - " + betas(nt) + " -> " + ret)
		ret
	}
}
