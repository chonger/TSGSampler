package parse.tsg.train
import scala.collection.mutable.{HashMap,DefaultEntry}
/**
 * TODO : 
 * figure out a good way to sample from the joint of a tree
 *
 */

class TSGTrainer(val treeDist : TreeDistribution, 
                 val data : Array[ParseTree with Markers],
                 val verbose : Boolean) {
    def this(td : TreeDistribution, data : Array[ParseTree with Markers]) = {
      this(td,data,false)
    }



	/**
     *  FOR DEBUG 
	 */
  
	def printtree(tree : ParseTree) = 
		println(PCFGPrinter.treeToString(treeDist.asInstanceOf[PCFGDistribution].pcfg,tree))  
	def ntStr(s : ParseTypes.Symbol) = {
	  treeDist.asInstanceOf[PCFGDistribution].pcfg.symbolStrings(s)
	}
	def printCounts() = {
		println("COUNTS")
		counts.elements.toList.sort((a,b) => {
			a._2._1 > b._2._1
		  }
		).foreach(_ match { case (tree,d) => if(d._1 > 0){printtree(tree);println(d)}})
	}
 
	def symb(s : String) = treeDist.asInstanceOf[PCFGDistribution].pcfg.symbolIDs(s)
	def term(s : String) = treeDist.asInstanceOf[PCFGDistribution].pcfg.terminalIDs(s)
 
/**	
	var mTree : ParseTree = {
	  
	  
	  new ParseTree(ProtoNode(symb("ROOT"),
                              List(ProtoNode(symb("A"),
                                             List(PreTerminalNode(symb("A"),TerminalNode(term("a"))),
                        		                  UnderspecifiedNode(symb("A"),null))))))
      
	  /**
	  new ParseTree(ProtoNode(symb("A"),
                           List(UnderspecifiedNode(symb("B"),null),
                        		UnderspecifiedNode(symb("B"),null))))
      */
      
	}
	var mTree2 : ParseTree = {
	  
	  
	  new ParseTree(ProtoNode(symb("ROOT"),
                              List(ProtoNode(symb("A"),
                                             List(PreTerminalNode(symb("A"),TerminalNode(term("a"))),
                        		                  PreTerminalNode(symb("A"),TerminalNode(term("a"))))))))
    }
*/
	type NonTerm = ParseTypes.Symbol 
	import scala.collection.mutable.{HashSet,HashMap}
	val rando = butil.Util.malletRandom //using mallet Randoms for sampling distributions
	var smoothFactor = 1.0 //default smoothing = none
	
	val sampleOrder : Array[Tuple2[ParseTree with Markers,NonTerminalNode with Parent]] = 
		data.flatMap(t => t.nonterminals.filter(n => !(n eq t.root)).map(nt => {
			(t,nt.asInstanceOf[NonTerminalNode with Parent])
		})).toArray
 
	/**
	 * A utility for setting all values of an array whose indices represent nts 
	 * to the value of a function of that nt
     */
	def ntset[A](arr : Array[A], setFn : (NonTerm) => A) = {
	  for{i <- 0 until arr.length} {
		  arr(i) = setFn(ParseTypes.intToSymbol(i)) 
	  }
	}
 
	
    var cache = new OpenHashMap[ParseTree,Tuple2[Int,Double]]()
    var counts = cache
//      var counts = new OpenHashMap[ParseTree,Tuple2[Int,Double]]() //counts of segments defined by the current sample
	var totals : Array[Int] = null	
 	
	var alphas : Array[Double] = null
	var betas : Array[Double] = null
 
	//one of these two methods MUST be called before train is called
	def initFromOther(sHM : OpenHashMap[ParseTree,Tuple2[Int,Double]],
                   	  iArr : Array[Int],
					  a : Array[Double],
					  b : Array[Double]) = {
	  counts.clear
      //println("LOADING")
	  sHM.elements.foreach(_ match {case (t,(c,d)) => counts += t -> (c,d)})
      //println(counts.size)
      //counts ++= sHM.elements
      
	  totals = new Array[Int](iArr.length)
	  for{i <- 0 to iArr.length - 1} {totals(i) = iArr(i)}
   
	  //these dont need to be copied
	  alphas = a 
	  betas = b
	}
 
	def initFromData() : Int = {
		//get the maximum index of a nonTerminal for sizing arrays
		var max : Int = 0
		data.foreach(_.nonterminals.foreach(nt => if(nt.symbol.toInt > max) max = nt.symbol.toInt))
		println("Got a maximum nt index of " + max)
		totals = new Array[Int](max + 1)
  
		val ALPHA_DEFAULT : Double = 100
		val BETA_DEFAULT : Double = .5
		betas = new Array[Double](max + 1) 
		ntset(betas,(nt : NonTerm) => {BETA_DEFAULT})
		alphas = new Array[Double](max + 1) 
		ntset(alphas,(nt : NonTerm) => {ALPHA_DEFAULT})
 
		data.foreach(t => {
			t.markers.foreach(n => {
				val segRef = new SegmentRef(n.n,t,n.n,false)
				increment(segRef,getScore(segRef))
			})
		})

		max
	}
 
 
	/**
     *  NO MORE INITIALIZATION AFTER THIS POINT
	 */
 
	//when a segment is not found, it must be turned into an actual tree to be entered
	def increment(tree : SegmentRef, score : Double) : Unit = increment(tree,score,1)
	def increment(tree : SegmentRef, score : Double, amount : Int) : Unit = {
      ///**
	  //if(verbose && tree == mTree) {
        //println("INC")
	    //printtree(tree.dumpSegment())
      
	  //println("A")
      var fromCounts = false
      val entry : Option[DefaultEntry[ParseTree,Tuple2[Int,Double]]] = {
        val cacheEntry = cache.fetchInternal(tree)
        if(cacheEntry.isEmpty) {
          val c = counts.fetchInternal(tree) 
          if(c.isDefined) {
            val gc = c.get
            cache += (gc.key -> gc.value)
          }
          c
        } else
          cacheEntry
      }
	  //println("A")
	  if(entry.isDefined) {
	    //println("already exists")
	    val oe = entry.get
	    //println("currently at " + oe.value)
	    //println("B")
	    oe.value = (oe.value._1 + amount,oe.value._2)
	    //counts += oe.key -> (oe.value._1 + 1,oe.value._2)
	    //println("B")
	    //println("NOW AT " + counts(tree))
	  } else { //put it in the cache
	    //println("new entry")
	    val dumpedSegment : MyHashTree = tree.dumpSegment()
	    //println("C")
	    cache += dumpedSegment -> (amount,score)
	    //println("C")
	  }
	  val nt = tree.root.symbol
	  totals(nt) = totals(nt) + amount
	  //println("DONE WITH INC")
	}
	def decrement(tree : SegmentRef) = {
      /**
      if(verbose && tree == mTree) {
        println("DEC")
	    printtree(tree.dumpSegment())
      }
*/
      //println("DEC")
	  //printtree(tree.dumpSegment())

      
	  val oe : DefaultEntry[ParseTree,Tuple2[Int,Double]] = 
        {
        val cacheEntry = cache.fetchInternal(tree)
        if(cacheEntry.isEmpty)
          counts.fetchInternal(tree).get
        else
          cacheEntry.get
      }
      
      /**
      val oe = try {
		val cacheEntry = cache.fetchInternal(tree)
        if(cacheEntry.isEmpty) {
          //println("GET FROM INTERNAL")
          counts.fetchInternal(tree).get
        } else
          cacheEntry.get
	  } catch {
	    case e : Exception => {
	      println("ERROR LOOKING UP THIS TREE")
	      printtree(tree.dumpSegment())
          printCounts()
	      throw e
	    }
	  }
      */
	  oe.value = (oe.value._1 - 1,oe.value._2)
   
	  /**
	  //this removes entries which reach zero count, also dumping their score.
	  if(oe.value._1 == 0) {
		  counts -= tree
	  }	
	  */

      /**
       * //check decrementing past zero
       * if(oe.value._1 < 0)
       * throw new Exception("Decremented too far")
       */
   
      //enable this if we never drop info
	  //val newCount = counts.getOrElse(tree,{printCounts;printtree(tree);throw new Exception()})
	  //NOTE : maybe we don't want to drop elements, as they save scores
	  //if(newCount._1 != 0)
	  //counts += tree -> (newCount._1 - 1,newCount._2)
	  //else
	  //	  counts.removeKey(tree)
	  //println("DONE WITH DEC")
	  val nt = tree.root.symbol
	  //totals(nt) = if(totals(nt) > 0) totals(nt) - 1 else 0
      totals(nt) = totals(nt) - 1
	}
 
	def getScore(segRef : SegmentRef) : Double = {
	  //println("SCORING")
	  //printtree(segRef.dumpSegment())
	  val score = treeDist.score(segRef)
	  //println("PCFG - " + score)
	  val ret = (score /: segRef.nonterminals)((a,b) => b match {
		  case un : UnderspecifiedNode => a * (1 - betas(b.symbol))
		  case _ => a * betas(b.symbol)
	  })
	  //println("GOT SCORE = " + ret)
	  ret
	}
 
	def getStats(tree : SegmentRef) : Tuple4[Int,Int,Double,Double]= {
	  val curCounts = counts.getOrElse(tree,(0,getScore(tree)))
	  (curCounts._1,
	   totals(tree.root.symbol.toInt),
	   alphas(tree.root.symbol.toInt),
	   curCounts._2)
	}
	
	def shuffle() = butil.Util.shuffle(sampleOrder)
 
	def train(smooth0 : Double, smooth1 : Double, iterations : Int) = {
	  for {i <- 1 to iterations} {
	    var smooth = smooth0 + (smooth1 - smooth0) * (i / iterations.toDouble)
	    smoothFactor = 1 / smooth
	    //println("Beginning Iteration " + i + " with smooth factor = " + smooth)  
	    resample()
	  }
	}
	
	def readSegments() = {
	  val segSet = HashSet[ParseTree]()
	  data.foreach(d => {
	    d.getSegments.foreach(segSet += _)
	  })
	  segSet.toList
	}
 	 
	/**
     *
     *                   NODE RESAMPLING
     * 
     */
 
	var iterTimes : List[Long]= Nil
    def resample() = {
		shuffle()
		val totalToSample = sampleOrder.length
		var numSampled = 0
		var totalTime : Long = 0
		var lastTime = System.nanoTime
		def printT(l : Long) = (((l / 10000000).toDouble) /  100.0).toString + " seconds" //prints number of seconds
		sampleOrder.foreach(_ match { case (tree,node) => {
		  resampleTreeAt(tree,node)
		  /**
		  numSampled += 1
          if(numSampled % 10000 == 0) {
            println(numSampled.toDouble / totalToSample.toDouble)
          }
*/
		  //if(numSampled % 100000 == 0) {
			  //butil.Util.analyzeHashMap(counts)
			  /**
		  	  var oldLastTime = lastTime
		  	  lastTime = System.nanoTime
		  	   var gap = lastTime - oldLastTime
		  	  totalTime += gap
		  	  var perc = numSampled.toDouble / totalToSample.toDouble
		  	  println((perc * 100).toString + "% - " + printT(gap) + " - " + printT(totalTime) + " Total")
		  	  println("Estimated iteration time - " + printT((totalTime.toDouble / perc).toLong))
		  	  */
		  //}

		}})
		//val time = System.nanoTime - lastTime
		//println("Iteration completed in " + printT(time))
		//iterTimes ::= time
		//val avgTime = ((0.0 /: iterTimes)(_ + _)) / iterTimes.length
		//println("Average Iteration time = " + printT(avgTime.toLong))
		//butil.Util.analyzeHashMap(counts)
	}
 
    def resampleTreeAt(tree : ParseTree with Markers, node : NonTerminalNode with Parent) : Unit = {
      /**
        if(verbose && tree.nodes.length == 6) {
		  println("RESAMPLING ")
		  printtree(tree)
		  println("at")
		  printtree(new ParseTree(node))
		  println("With segments")
		  tree.getSegments.foreach(printtree(_))
        }
        */
		  //printCounts()
		  
		  val parent = node.parent
	      val isMarkedNow = tree.isMarked(node)
	      //println(isMarkedNow)
	      val parentSeg = new SegmentRef(parent,tree,node,isMarkedNow)
		  //println("ParentSeg")
		  //printtree(parentSeg.dumpSegment())
		  //decrement a SegmentRef from parent with node marked as the same
    
		  //the parent segment is definitely in the counts, we could just save a pointer to that segment
		  decrement(parentSeg)
		  //get a SegmentRef U from this node with node marked as unmarked so it will expand
		  val uSeg = new SegmentRef(node,tree,node,false)
		  val(above,below,joined) = if(isMarkedNow) {
			decrement(uSeg)
		    //get a SegmentRef V from the parent with this node unmarked
		    val vSeg = new SegmentRef(parent,tree,node,false)
		    //sample using parent as above, U as below, V as join
		    (parentSeg,uSeg,vSeg)
		  } else {
		    //get a SegmentRef V from parent with this node marked
		    val vSeg = new SegmentRef(parent,tree,node,true)
		    //sample using V as above, U as below, parent as join
		    (vSeg,uSeg,parentSeg)
		  }
		  //printCounts()
          
		//
		//count and prob should be stored in the same hashmap
		//alpha and totals are an array lookup for a node
		//prob must be calculated, but should be stored for previously cacluated segments
		//println("GETTING STATS")
		val (jCount,jTotal,jAlpha,jProb) = getStats(joined)
		val (aCount,aTotal,aAlpha,aProb) = getStats(above)
		val (bCount,bTotal,bAlpha,bProb) = getStats(below)
		//println("DONE GETTING STTS")
		//the above == below here requires a new equals for comparing SegmentRefs
		val treeKronDel = if(above == below) 1 else 0
		
		//this is ok as SegmentRefs extend Parse trees with the right root
		val ntKronDel = if(above.root.symbol == below.root.symbol) 1 else 0
  
		val unSmoothedJ = (jCount + jAlpha * jProb)/(jTotal + jAlpha)
		val joinProb = Math.pow(unSmoothedJ,smoothFactor)
		val aboveProb = (aCount + aAlpha * aProb)/(aTotal + aAlpha)
		val belowProb = (treeKronDel + bCount + bAlpha * bProb)/(bTotal + bAlpha + ntKronDel)
		val combProb = Math.pow(aboveProb * belowProb,smoothFactor)
		
		/**
		if(joinProb == 0 || aboveProb == 0 || belowProb == 0) {
		    printtree(above)
		    printtree(below)
		    printtree(joined)
			println(getStats(joined))
			println(getStats(above))
			println(getStats(below))
			throw new Exception
		}
        */
		 /**
		if(Math.abs(treeDist.score(joined) - treeDist.score(above) * treeDist.score(below)) > .0001) {
		    printtree(above)
		    println(treeDist.score(above))
		    printtree(below)
		    println(treeDist.score(below))
		    printtree(joined)
		    println(treeDist.score(joined))
			throw new Exception
		}
		*/
		val total = combProb + joinProb
		/**
		if(below == mTree && aCount != 0 && bCount != 0 && jCount == 0) {
			printtree(above)
			printtree(below)
			printtree(joined)
			
			println(getStats(above))
			println(getStats(below))
			println(getStats(joined))
			println("US J vs C  " +unSmoothedJ + " vs " + (aboveProb * belowProb) )
			println(bProb + " <> " + belowProb)
			println((bAlpha * bProb)/(bTotal + bAlpha))
			println("JOIN vs COMB = " + joinProb + " vs " + combProb)
			println("Mark prob = " + (combProb / total))
		}
		*/
		/**
	    if(above == mTree && aCount > 3) {
	    	printtree(above.dumpSegment())
			printtree(below.dumpSegment())
			printtree(joined.dumpSegment())
			println("STATS - count, total,alpha,prob")
			println(getStats(above))
			println(getStats(below))
			println(getStats(joined))
			println("US J vs C  " +unSmoothedJ + " vs " + (aboveProb * belowProb) )
			println(aProb + " <> " + aboveProb)
			println(bProb + " <> " + belowProb)
			println(jProb + " <> " + joinProb)
			println((aAlpha * aProb)/(aTotal + aAlpha))
			println("JOIN vs COMB = " + joinProb + " vs " + combProb)
			println("Mark prob = " + (combProb / total))
		}
		*/
		/**
		println("SAMPLE")
		printCounts()
		printtree(above.dumpSegment())
		println(getStats(above))
		printtree(below.dumpSegment())
		println(getStats(below))
		printtree(joined.dumpSegment())
		println(getStats(joined))
		println("JOIN vs COMB = " + joinProb + " vs " + combProb) 
		println("Mark prob = " + (combProb / total))
		*/
		
		val sampleMark = if(rando.nextDouble() > (joinProb / total)) {
			//cutCount += 1
			//cutTotal += 1
			true
		} else {	
			//cutTotal += 1
			false
		}
    
		if(sampleMark) { //split
		  increment(above,aProb)
		  increment(below,bProb)
		  tree.mark(node)
		  tree.updateParentsFromMarkAt(node)
		} else { //join
		  increment(joined,jProb)
		  tree.unmark(node)
		  tree.updateParent(node,parent)   
		}
		//printCounts()
    }
}
