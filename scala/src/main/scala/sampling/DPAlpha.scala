package sampling

object DPAlpha {

	val rando = butil.Util.malletRandom
  
	def resampleAlphaEscobarWest(k : Int, 
                              	 n : Int, 
                              	 curAlpha : Double, 
                              	 a : Double, 
                              	 b : Double) : Double = {
		if(k <= 1)
			return curAlpha
		val bn = rando.nextBeta(curAlpha + 1,n)
		val blesslog = a - Math.log(bn)
		val ratio = (a + k - 1) / (n * blesslog)
		val pi = ratio / (1 + ratio)
		
		if(rando.nextDouble() <= pi) {
			rando.nextGamma(a + k,blesslog)
		} else {
			rando.nextGamma(a + k - 1,blesslog)
		}
	}  
  
	def resampleAlphaMetropolisH(k : Int, 
                              	 n : Int, 
                              	 curAlpha : Double, 
                              	 a : Double, 
                              	 b : Double,
								 variance : Double) : Double = {
		
		def gamma = cc.mallet.util.Maths.gamma _
		def beta = cc.mallet.util.Maths.beta _
  
		def evalGammaDist(d : Double) : Double = {
			var ret = 1 / (Math.pow(b,a) * gamma(a))
			ret * Math.pow(d,a - 1) * Math.exp(-d / b)
		}
		def evalBetaDist(d : Double, al : Double, bt : Double) : Double = {
			1 / beta(al,bt) * Math.pow(d,al - 1) * Math.pow(1-d,bt - 1)
		} 
		def evalPosterior(alpha : Double) : Double = {
			evalGammaDist(alpha) * Math.pow(alpha,k-1) * (alpha + n) * evalBetaDist(alpha,alpha + 1,n) 
		}
  
		def evalLogNormal(d : Double, mu : Double, sigSq : Double) : Double = {
			val frac = 1.0 / (d * Math.sqrt(2.0 * Math.Pi * sigSq))
			//println("frac = " + frac)
			val exponent = -1.0 * Math.pow(Math.log(d) - mu,2.0) / (2.0 * sigSq)
			//println("EXP = " + exponent)
			//println("RAISED = " + Math.exp(exponent))
			frac * Math.exp(exponent)
		}
		
		def getLNMeanVar(alpha : Double) : Tuple2[Double,Double] = {
			val vari = Math.log((variance / Math.pow(alpha,2)) + 1)
			val mean = Math.log(alpha) - (vari / 2)
			(mean,vari)
		}
  
		def sampleLogNorm(mu : Double, vari : Double) = 
			Math.exp(mu + vari * rando.nextGaussian(0,1))
		
		val (caMean,caVar) = getLNMeanVar(curAlpha) 
		
		val nextAlpha = sampleLogNorm(caMean,caVar)
		 
		val (naMean,naVar) = getLNMeanVar(nextAlpha)
		
		//println("SAMPLED " + nextAlpha + " from ALDIST " + curAlpha)
		var qFrac = evalLogNormal(curAlpha,naMean,naVar)
		//println("It had probability " + evalLogNormal(nextAlpha,caMean,caVar))
		qFrac = qFrac / evalLogNormal(nextAlpha,caMean,caVar)
		var pFrac = evalPosterior(nextAlpha) / evalPosterior(curAlpha)
		val accept = pFrac * qFrac
		//println(pFrac)
		//println(qFrac)
		//println("ACCEPT = " + accept)
		if(accept >= 1)
			nextAlpha
		else {
			if(rando.nextDouble() > accept) {
				curAlpha
			} else
				nextAlpha
		}
	}
  
}
