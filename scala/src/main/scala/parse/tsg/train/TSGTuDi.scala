package parse.tsg.train

import scala.actors.Actor

class TSGTuDi(shifu : Actor, val verbose : Boolean) extends Actor {
  
    def this(shifu : Actor) = {
      this(shifu,false)
    }

	var jobName : String = "None"
	var sampleData : SampleData = null
    
	def act() {
	  //println("TuDi " + jobName + " is Acting")
	  if(shifu == null)
		  throw new Exception()
	  react {
	    case (jobName : String, data : SampleData) => {   
	      //println("Got a jobname = " + jobName + ", and a map with " + data.counts.size + " elems")
          
	      val trainer = new TSGTrainer(data.treeDist,data.trees.toArray,verbose)
          
	      trainer.initFromOther(data.counts,data.totals,data.alphas,data.betas)
	      
          this.jobName = jobName 
	      this.sampleData = data
	      
          //println("TuDi " + jobName + " is ready to sample " + data.trees.length + 
          //       " trees for " + data.iterations + " iterations")
          
	  	  trainer.train(data.smooth0,data.smooth1,data.iterations)
          //println("TuDi " + jobName + " is composing its reports")
	  	  val myCounts = new OpenHashMap[ParseTree,Tuple2[Int,Double]]()
	  	  val myTotals = new Array[Int](trainer.totals.length)
	  	  trainer.data.foreach(tree => {
	  			tree.getSegments.foreach(seg => {
	  			  val count = myCounts.getOrElse(seg,(0,trainer.cache(new MyHashTree(seg.root))._2))
	  			  val newStore = (count._1 + 1,count._2)
	  			  myCounts += seg -> newStore
	  			  myTotals(seg.root.symbol) = myTotals(seg.root.symbol) + 1
	  			})
	  		})
	  		shifu ! (new SampleResult(myCounts,trainer.data,myTotals),this)
	    }
	  	case err => {
	  	  throw new Exception("Unhandled TuDi message " + err)
	  	} 
	  }
	}
}
