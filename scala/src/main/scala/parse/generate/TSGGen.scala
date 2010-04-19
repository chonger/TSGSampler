package parse.generate

object TSGGen {
  def main(args : Array[String]) : Unit = {
    val filename = args(0)
    val outfile = args(1)
    val numToGen = args(2).toInt
    val maxNodes = args(3).toInt
    
    val jwock = Jabberwock.createFromFile(filename)
    val data = for {i <- 1 to numToGen} yield {
      println(i)
      var tree = jwock.genTree(Some(100))
      while(tree.nodes.length > maxNodes)
        tree = jwock.genTree(Some(100))
      tree
    }
    corpora.treebank.TreebankData.write(outfile,data.toList,jwock.pcfg)
     val pcfg = new DirectlyEstimatedPCFG()
    val raw = corpora.treebank.TreebankData.read(outfile,pcfg)
    val (syms,terms) = pcfg.size
    println("PCFG created with " + syms + " symbols and " + terms + " terminals")
    print("Estimating PCFG ... ")
    pcfg.process(raw)
    println("done")
    
    println("KL Divergence = " + jwock.klDiv(pcfg))
  }
}
