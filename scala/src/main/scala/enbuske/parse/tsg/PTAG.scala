package enbuske.parse.tsg

import scala.collection.mutable.{HashMap,HashSet}
import scala.collection.mutable.Stack
import io.Source

object PTAG {

  def fromFile(filename : String) = {
    
    println("Loading TAG from " + filename)
    val pcfg = new PCFG()

    val tsgMap = new HashMap[ParseTree,Int]();
    val tsgdata = Source.fromFile(filename + ".tsg").getLines
    val tsgSegments = tsgdata.map(_.split("\t")).toList
    println("Got " + tsgSegments.length + " segments")
    tsgSegments.map(a => {
      tsgMap += pcfg.growTree(a(0)) -> a(1).toInt
    })

    val tagMap = new HashMap[ParseTree,Int]();
    val tagdata = Source.fromFile(filename + ".tag").getLines
    val tagSegments = tagdata.map(_.split("\t")).toList
    println("Got " + tagSegments.length + " segments")
    tagSegments.map(a => {
      tagMap += pcfg.growTree(a(0)) -> a(1).toInt
    })

    val tagProb = new HashMap[ParseTypes.Symbol,Double]()
    val probData = Source.fromFile(filename + ".tagProb").getLines
    tsgdata.map(_.split("\t")).foreach(a => {
      tagProb += pcfg.symbolIDs(a(0)) -> a(1).toDouble
    })
    
    val tagAMap = new HashMap[String,Double]()
    val tagAData = Source.fromFile(filename + ".tagAlpha").getLines
    tagAData.map(_.split("\t")).foreach(a => {
      tagAMap += a(0) -> a(1).toDouble
    })
    val tagAlpha = pcfg.symbolStrings.map(a => {
      tagAMap(a)
    }).toArray

    val tsgAMap = new HashMap[String,Double]()
    val tsgAData = Source.fromFile(filename + ".tsgAlpha").getLines
    tsgAData.map(_.split("\t")).foreach(a => {
      tsgAMap += a(0) -> a(1).toDouble
    })
    val tsgAlpha = pcfg.symbolStrings.map(a => {
      tsgAMap(a)
    }).toArray

    val tsgBMap = new HashMap[String,Double]()
    val tsgBData = Source.fromFile(filename + ".tsgBeta").getLines
    tsgBData.map(_.split("\t")).foreach(a => {
      tsgBMap += a(0) -> a(1).toDouble
    })
    val tsgBeta = pcfg.symbolStrings.map(a => {
      tsgBMap(a)
    }).toArray

    val tagBMap = new HashMap[String,Double]()
    val tagBData = Source.fromFile(filename + ".tagBeta").getLines
    tagBData.map(_.split("\t")).foreach(a => {
      tagBMap += a(0) -> a(1).toDouble
    })
    val tagBeta = pcfg.symbolStrings.map(a => {
      tagBMap(a)
    }).toArray

    val tsgBase = new CohnGoldwater(pcfg,tsgBeta)
    val tagBase = new CohnGoldwater(pcfg,tagBeta)
     
    new PTAG(pcfg,tsgMap,tagMap,tagProb,tsgBase,tagBase,tsgAlpha,tagAlpha)
  }

}


class PTAG(val pcfg : PCFG, 
           tsgCounts : HashMap[ParseTree,Int],  
           tagCounts : HashMap[ParseTree,Int],  
           val tagProb : HashMap[ParseTypes.Symbol,Double],  
           tsgBase : TreeDistribution, 
           tagBase : TreeDistribution, 
           tsgAlphas : Array[Double],
           tagAlphas : Array[Double]) {

  val tsgDP = new ETreeDP(pcfg,tsgCounts,tsgBase,tsgAlphas)
  val tagDP = new ETreeDP(pcfg,tagCounts,tagBase,tagAlphas)

  /**
  def generate() : ParseTree = {
    new ParseTree(recGen(new UnderspecifiedNode(pcfg.symbolIDs("ROOT"),null)))
  }

  def recGen(un : UnderspecifiedNode) : NonTerminalNode = {
    
    //first find a TSG rule
    val nextTree = tsgDP.sample(un.symbol).deepCopy()
    
    def recTAG(n : NonTerminalNode) : NonTerminalNode = {
      n match {
        case pt : PreTerminalNode => { //no tagging or substitution here
          new PreTerminalNode(pt.symbol,new TerminalNode(pt.kid.terminal))
        }
        case un : UnderspecifiedNode => {
          recGen(un)
        }
        case pn : ProtoNode => {
          val r = util.Util.javaRandom.nextDouble
          if(r > tagProb(pn.symbol) && tagDP.headMap(pn.symbol).length > 0) { //TAG!
            val tagSeg = tagDP.headMap(pn.symbol)
            
          }
        }
      }
    }

    recTAG(nextTree.root)

  }
*/

}

