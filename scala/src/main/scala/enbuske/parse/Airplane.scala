package enbuske.parse

import tsg._
import java.io.{BufferedWriter,FileWriter,File}
import scala.collection.mutable.HashMap

class Airplane(val base : String) {

  val pcfg = new DirectlyEstimatedPCFG()

  val wsjtrees = {
    val tfile = base + "/train.small.txt"
    (pcfg.read(tfile)).map((r => new ParseTree(r.root) with Markers with Aspect))
  }
  val airtrees = {
    val tfile = base + "/airplane.small.txt"
    pcfg.read(tfile).map((r => new ParseTree(r.root) with Markers with Aspect))
  }

  val alltrees = wsjtrees ::: airtrees
  val tsets = List(wsjtrees.toArray,airtrees.toArray)

  pcfg.process(alltrees)


  def packHDP() = {
    val packer = new TSGPackager(pcfg)
    
    val conn : List[List[Double]] = List(List(1,0),
                                         List(.5,.5))                

    packer.packageHDP(tsets.map(_.toList),2,conn,base + "/airhdp.pack")

  }

  def readHDP() : HDPTSG = {
    
    val packer = new TSGPackager(pcfg)

    val hdptsg : HDPTSG = packer.unpackHDP(tsets,base + "/airhdp.cpack")
    hdptsg.addPCFGRules(alltrees)
    hdptsg
  }


  def writeHDP() = {
    val hdptsg = readHDP()

    //println( hdptsg.scoreMap)
    val wsjScores = hdptsg.scoreMap(0)

    val airScores = hdptsg.scoreMap(1)

    var airsegs : List[List[ParseTree]] = Nil
    var wsjsegs : List[List[ParseTree]] = Nil

    def aScor(s : ParseTree) : Double = {
      airScores(s) * hdptsg.mixWeights(1)(1)(s.root.symbol)
    }

    def wScor(s : ParseTree) : Double = {
      wsjScores(s) * hdptsg.mixWeights(1)(0)(s.root.symbol)
    }

    pcfg.symbolStrings.foreach(s => { //for each symbol
      var aSegs : List[ParseTree] = Nil
      var wSegs : List[ParseTree] = Nil
      val segs = hdptsg.headMap(pcfg.symbolIDs(s))

      segs.foreach(s => {
        val aScr = aScor(s)
        val wScr = wScor(s)
        if(aScr > wScr)
          aSegs ::= s
        else
          wSegs ::= s
      })

      airsegs ::= aSegs
      wsjsegs ::= wSegs
      println("GOT " + aSegs.length + " AIRSEGS")
      println("GOT " + wSegs.length + " WSJSEGS")

    })
    airsegs = airsegs.reverse
    wsjsegs = wsjsegs.reverse 

    val aScrSeg = airsegs.map(as => {
      as.map(seg => {
        val airScr = airScores(seg)
/**
        if(seg.nonterminals.length > 10) {
          println(PCFGPrinter.treeToString(pcfg,seg))
          println(airScr)
        }
   */     
        //(seg,aScor(seg) / wScor(seg))
        (seg,airScores(seg))
        
      }).filter(_ match {
        case (seg,ratio) => {
          aScor(seg) > .000001
        }
      })
    })
    val wScrSeg = wsjsegs.map(ws => {
      ws.map(seg => {
        //(seg,wsjScores(seg) / airScores(seg))
        (seg,wScor(seg))
      })
    })

    val wsjbw = new BufferedWriter(new FileWriter(new File(base + "/wsj.seg")))
    val airbw = new BufferedWriter(new FileWriter(new File(base + "/air.seg")))

    aScrSeg.foreach(as => {
      as.sort((a,b) => {a._2 > b._2}).slice(0,10).foreach(e => {
        airbw.write(PCFGPrinter.treeToString(pcfg,e._1) + "\n")
        airbw.write(aScor(e._1) + "\n")
        airbw.write(e._2 + "\n")
      })
       airbw.write("\n------------------------------------------------\n")
    })
    
    wScrSeg.foreach(ws => {
      ws.sort((a,b) => {a._2 > b._2}).slice(0,10).foreach(e => {
        wsjbw.write(PCFGPrinter.treeToString(pcfg,e._1) + "\n")
        wsjbw.write(e._2 + "\n")
      })
      
      wsjbw.write("\n------------------------------------------------\n")
    })

    wsjbw.close()
    airbw.close()

/**
    writeAll(wsjScores,hdptsg.headMap,base + "/wsj.seg")
    writeAll(airScores,hdptsg.headMap,base + "/airplane.seg")
*/
  }

  def writeAll(scoreMap : HashMap[ParseTree,Double], headMap : Array[List[ParseTree]], filename : String) = {
    val bw = new BufferedWriter(new FileWriter(new File(filename)))
    pcfg.symbolStrings.foreach(s => { //for each symbol
      val segs = headMap(pcfg.symbolIDs(s)) //
   
      

      segs.sort((a,b) => {scoreMap(a) > scoreMap(b)}).slice(0,10).foreach(e => {
        bw.write(PCFGPrinter.treeToString(pcfg,e) + "\n")
        bw.write(scoreMap(e) + "\n")
      })

      bw.write("Z = " + (0.0 /: segs)((a,b) => a + scoreMap(b)) + "\n------------------------------------------------\n")
    })
    bw.close()
  }

}
