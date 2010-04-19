package parse.tsg.train

import scala.actors.Actor

class SampleData(val treeDist : TreeDistribution,
				 val trees : List[ParseTree with Markers],
                 val smooth0 : Double,
                 val smooth1 : Double, 
                 val iterations : Int, 
                 val counts : OpenHashMap[ParseTree,Tuple2[Int,Double]],
				 val totals : Array[Int],
				 val alphas : Array[Double],
				 val betas : Array[Double])

class SampleResult(val counts : OpenHashMap[ParseTree,Tuple2[Int,Double]],
                   val trees : Array[ParseTree with Markers],
				   val totals : Array[Int])

class TSGShiFu(val trainer : TSGTrainer with ConcentrationParameterResampling, numTuDi : Int, shifu : Actor) extends Actor {
  
  /**
   * This constructor is used for now, the other one can be used to tree out threads
   * but that wont work at all yet
   */
  def this(trainer : TSGTrainer with ConcentrationParameterResampling, numTuDi : Int) = this(trainer,numTuDi,null)
  
  trainer.initFromData
  //shorthand for getting to trainers counts...maybe make this class extend TSGTrainer
  def counts = trainer.counts
  
  //the tudi are created automatically here
  val tuDiMen : List[TSGTuDi] = (for{i <- 1 to numTuDi} yield {
    if(i == 2)
      new TSGTuDi(this,true)
    else
      new TSGTuDi(this,false)
  }).toList 
  
  var smooth0 = 1.0
  var smooth1 = 1.0
  var iterations = 100
  var iter = 0
  var totalTime : Long = 0
  var lastTime = System.nanoTime
  var iterTimes : List[Long] = Nil
  def printT(l : Long) = (((l / 10000000).toDouble) /  100.0).toString + " seconds" //prints number of seconds
		
  val working = scala.collection.mutable.HashSet[TSGTuDi]()
  val waiting = scala.collection.mutable.HashSet[TSGTuDi]()
  waiting ++= tuDiMen
  
  def train(smooth0 : Double, smooth1 : Double, iterations : Int) : Unit = {
    if(iterations <= 0) {
    	if(shifu != null)
    		shifu ! new SampleResult(trainer.counts,trainer.data,trainer.totals)
    	return
    }
    this.smooth0 = smooth0
    this.smooth1 = smooth1
    this.iterations = iterations
    
    
    //for each tree subset, get the sample list
    /**
     * leave this to the TuDi
    val sampleLists = treeLists.map(_.flatMap(t => 
      t.nonterminals.filter(n => !(n eq t.root)).map(nt => {
			(t,nt.asInstanceOf[NonTerminalNode with Parent])
      })))
    */
      
    iter = 0
    totalTime = 0
    iterTimes = Nil
    lastTime = System.nanoTime
    dispatchTuDi    
    start()
    
  }
  
  def dispatchTuDi() = {
    
    val total = trainer.data.length
    val sampleSize = (total / numTuDi)
    val plusOneOffset : Int = total - sampleSize * numTuDi
    
    //split the parse trees into subsets with size difference <= 1
    val treeLists : List[List[ParseTree with Markers]] = {
      var ret1 = if(plusOneOffset > 0) {
    	  (for{i <- 0 to plusOneOffset-1} yield {
    		  val start = i * (sampleSize + 1)
    		  (for {j <- start to (start + sampleSize)} yield {trainer.data(j)}).toList
    	  }).toList
      } else Nil
      val ret =  
        (for{i <- 0 to numTuDi - 1 - plusOneOffset} yield {
    	  val start = plusOneOffset * (sampleSize + 1) + i * sampleSize
    	  (for {j <- start to (start + sampleSize - 1)} yield {trainer.data(j)}).toList
        }).toList
      
      ret ::: ret1
    }
    
    //println("Sending off " + tuDiMen.length + " TuDi")
    working ++= waiting
    waiting.clear
   
    var smooth = smooth0 + (smooth1 - smooth0) * (iter / iterations.toDouble)
    
    tuDiMen.foreach(_.start())
    (tuDiMen zip (1 to numTuDi).toList zip treeLists).foreach(
      _ match {case ((td,name),tl) => td ! (name.toString,
                                            new SampleData(trainer.treeDist, //we could swap distributions...
                                            			   tl, 
                                                           smooth, 
                                                           smooth,
                                                           1, //just one iteration
                                                           trainer.cache, //use this trainer's cache
                                                           trainer.totals,
                                                           trainer.alphas,
                                                           trainer.betas
                                            ))})
  }
  
  def act() = {
    
    val newCounts = new OpenHashMap[ParseTree,Tuple2[Int,Double]] 
    
    //TODO : also set nt counts to zero
    //record all the trainers which we expect replies from
    var continue = true
     while(continue) {
    	//println("ShiFu waiting to receive")
	    receive {
	      case (result : SampleResult, tr : TSGTuDi) => {
	        //println("ShiFu is analyzing reports from " + tr.jobName)
	        result.counts.keys.foreach(tree => result.counts(tree) match { 
	          case (count,score) => {
	        	  val myTree = new MyHashTree(tree.root)
	        	  val result = newCounts.getOrElse(myTree,(0,score))
	        	  newCounts += myTree -> (result._1 + count,result._2)
	          }
	        })
	        working -= tr
	        waiting += tr
	        if(working.isEmpty) {
	        	//println("All TuDi have reported in for iteration " + iter)
	        	trainer.cache.clear //this gets rid of previous calculations of scores.
	        	trainer.cache ++= newCounts.elements
	        	//trainer.printCounts()
	        	newCounts.clear
	        	
          
          
	        	val curTime = System.nanoTime
	        	val lapse = curTime - lastTime
	        	iterTimes ::= lapse
	        	val avgTime = ((0.0 /: iterTimes)(_ + _)) / iterTimes.length
	        	lastTime = curTime
	      
	        	println("Iteration " + iter + " completed in " + printT(lapse))
	        	println("Average Iteration time = " + printT(avgTime.toLong))
	        	iter += 1
	        	if(iter >= iterations) {
	        		println("Shifu Finished")
	        		if(shifu != null)
	        			shifu ! new SampleResult(trainer.cache,trainer.data,trainer.totals)
	        		continue = false
	        	} else {
	        	  trainer.sampleAlpha
	        	  trainer.sampleBeta
	        	  dispatchTuDi()
	        	}
	        }
	      }
	      case err => throw new Exception("Unhandled message to ShiFu : " + err)
	    }
    }
  }
}
