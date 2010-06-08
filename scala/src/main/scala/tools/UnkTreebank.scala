package tools

import parse._
import corpora.treebank._

object UnkTreebank {
  def main(args : Array[String]) : Unit = {
    val pcfg = new PCFG
	val data = TreebankData.read(args(0),pcfg)
	
    val unker =  new UnkFromPCFG(pcfg)

    val newdata = TreebankData.read(args(1),pcfg)
	val (unkbank,unkPCFG) = unker.unkData(newdata,pcfg)
	TreebankData.write(args(2),unkbank,unkPCFG)
  }
}
