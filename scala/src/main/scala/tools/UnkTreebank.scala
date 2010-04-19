package tools

import parse._
import corpora.treebank._

object UnkTreebank {
  def main(args : Array[String]) : Unit = {
    val pcfg = new PCFG
	val data = TreebankData.read(args(0),pcfg)
	val (unkbank,unkPCFG) = new UnkLeastCommon(5,data,pcfg).unkData(data,pcfg)
	TreebankData.write(args(1),unkbank,unkPCFG)
  }
}
