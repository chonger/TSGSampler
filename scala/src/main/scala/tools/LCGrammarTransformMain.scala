package tools


object LCGrammarTransformMain {
  def main(args : Array[String]) : Unit = {
    /**
    val usage = "usage : [-f (forward) or -b (backwards) -k (checkk)] [infile] [outfile]"
    
    if(args.length < 3)
      throw new Exception(usage)
    val data = TreebankData.read(args(1))
    val orig = data
    //val data = orig.map(t => t.copy).toList
    val flag = args(0)
    if(List("-f","-k") contains flag) {	   
    	var pcfg = new PCFG with PrintablePCFG
						with LeftCornerTransform with LeftFactorizeBar
		pcfg.growTrees(data)
		pcfg.transform(data)
		TreebankData.write(args(2),data,pcfg)
		if(flag == "-k") {
			pcfg.revert(data)
			if(data != orig)
				println("Does not check out")
			else {
				println("Checks out")
				orig.foreach(d => println(pcfg.printTree(d.tree)))
				data.foreach(d => println(pcfg.printTree(d.tree)))	
			}
		}
    
    } else if (args(0) == "-b") {
    	val pcfg = new PCFG with PrintablePCFG with LeftFactorizeBar
    	pcfg.growTrees(data)
    	pcfg.transform(data)  //turns it into binary nodes, not protoNodes
     
    	var lcPcfg = new PCFG(pcfg) with PrintablePCFG
					with LeftCornerTransform with LeftFactorizeBar 
		lcPcfg.revert(data)
		TreebankData.write(args(2),data,lcPcfg)
    } else 
      throw new Exception(usage)
    */
  }
  
}