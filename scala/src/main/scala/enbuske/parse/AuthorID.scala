package enbuske.parse

import tsg._

class AuthorID(val base : String) {

  val authors : List[String] = io.Source.fromFile(base + "/info.txt").getLines.toList.map(_.replaceAll("\\s",""))

  val pcfg = new DirectlyEstimatedPCFG()

  val traintrees : List[ParseTree with Markers] = authors.flatMap(a => {
    val tfile = base + "/" + a + "/train/all.train.txt"
    (pcfg.read(tfile)).map((r => new ParseTree(r.root) with Markers))
  })
  val testtrees : List[ParseTree with Markers]= authors.flatMap(a => {
    val tfile = base + "/" + a + "/test/all.test.txt"
    pcfg.read(tfile).map((r => new ParseTree(r.root) with Markers))
  })

  val alltrees = traintrees ::: testtrees

  pcfg.process(alltrees)

  def packHDP() = {
    val packer = new TSGPackager(pcfg)
    
    val tsets = authors.map(a => {
      val tfile = base + "/" + a + "/train/all.train.txt"
      pcfg.read(tfile).map((r => new ParseTree(r.root) with Markers with Aspect))
    })

    val conn : List[List[Double]] = List(List(1,0,0,0,0,0),
                                         List(0,1,0,0,0,0),
                                         List(0,0,1,0,0,0),
                                         List(0,0,0,1,0,0),
                                         List(0,0,0,0,1,0),
                                         List(0,0,0,0,0,1))
                    

    packer.packageHDP(tsets,6,conn,base + "/hdp.pack")

  }

  def readHDP() : HDPTSG = {
    
    val packer = new TSGPackager(pcfg)
    
    val tsets = authors.map(a => {
      val tfile = base + "/" + a + "/train/all.train.txt"
      pcfg.read(tfile).map((r => new ParseTree(r.root) with Markers with Aspect)).toArray
    })

    val hdptsg : HDPTSG = packer.unpackHDP(tsets,base + "/hdp.cpack")

    hdptsg
  }


  def packAll() = {
    val packer = new TSGPackager(pcfg)
    
    authors.foreach(a => {
      val tfile = base + "/" + a + "/train/all.train.txt"
      val trees = pcfg.read(tfile).map((r => new ParseTree(r.root) with Markers))
      packer.packageOne(trees,base + "/" + a + "/train/all.train.pack")
    })

  }

  def classifyAll() = {

    val packer = new TSGPackager(pcfg)
    val ptsgs = authors.map(a => {
      val tfile = base + "/" + a + "/train/all.train.txt"
      val trees = pcfg.read(tfile).map((r => new ParseTree(r.root) with Markers))
      packer.unpack(trees.toArray,base + "/" + a + "/train/all.train.cpack")
    })

    ptsgs.foreach(_.addPCFGRules(alltrees))

    var correct = 0.0
    var total = 0.0

    var aInd = 0
    authors.foreach(a => {
      val testFs = io.Source.fromFile(base + "/" + a + "/test/info.test.txt").getLines.toList.map(_.replaceAll("\\s",""))
      
      println("LOOKING AT AUTHOR " + a)

      testFs.foreach(testDoc => {
        
        val trees = pcfg.read(base + "/" + a + "/test/" + testDoc)

        val scores = ptsgs.map(ptsg => {
          (1.0 /: trees)((a,b) => {
            var scr = ptsg.scoreTree(b)
            if(scr == 0) {
              scr = Math.pow(10,-300)
            }
            a + Math.log(scr)
          })
        })

  
        
        var maxInd = 0
        var maxVal = -100000000000.0
        var ind = 0
        println(scores)
        scores.foreach(scr => {
          if(scr > maxVal) {
            maxVal = scr
            maxInd = ind
          }
          ind += 1
        })

        var tInd = 0
        var tVal = -10000000000000.0
        ind = 0
        scores.foreach(scr => {
          if(ind != maxInd && scr > tVal) {
            tVal = scr
            tInd = ind
          }
          ind += 1
        })

        println("Classified as " + authors(maxInd))
        println("Runner up " + authors(tInd))

        total += 1
        if(maxInd == aInd)
          correct += 1
      })

      aInd += 1
    })

    println("Accuracy = " + (correct / total))


  }


  def classifyAllHDP() = {

    val hdptsg = readHDP()

    hdptsg.addPCFGRules(alltrees)

    var correct = 0.0
    var total = 0.0

    var aInd = 0
    authors.foreach(a => {
      val testFs = io.Source.fromFile(base + "/" + a + "/test/info.test.txt").getLines.toList.map(_.replaceAll("\\s",""))
      
      println("LOOKING AT AUTHOR " + a)

      testFs.foreach(testDoc => {
        
        val trees = pcfg.read(base + "/" + a + "/test/" + testDoc)

        val scores = (for(i <- 0 to hdptsg.numDP - 1) yield {
          (0.0 /: trees)((a,b) => {
            var scr = hdptsg.scoreTree(b,i)
            if(scr == 0) {
              scr = Math.pow(10,-300)
            }
            a + Math.log(scr)
          })
        }).toList
        
        var maxInd = 0
        var maxVal = -100000000000.0
        var ind = 0
        println(scores)
        scores.foreach(scr => {
          if(scr > maxVal) {
            maxVal = scr
            maxInd = ind
          }
          ind += 1
        })

        var tInd = 0
        var tVal = -10000000000000.0
        ind = 0
        scores.foreach(scr => {
          if(ind != maxInd && scr > tVal) {
            tVal = scr
            tInd = ind
          }
          ind += 1
        })

        println("Classified as " + authors(maxInd))
        println("Runner up " + authors(tInd))

        total += 1
        if(maxInd == aInd)
          correct += 1
      })

      aInd += 1
    })

    println("Accuracy = " + (correct / total))
  }

}
