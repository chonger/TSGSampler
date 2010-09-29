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

    println( hdptsg.scoreMap)
    val wsjScores = hdptsg.scoreMap(0)

    val airScores = hdptsg.scoreMap(1)


    writeAll(wsjScores,hdptsg.headMap,base + "/wsj.seg")
    writeAll(airScores,hdptsg.headMap,base + "/airplane.seg")

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
