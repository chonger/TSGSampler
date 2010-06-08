package tools

import parse._
import corpora.treebank._
import parse.transform._

/**
 *  Gets all trees with terminal yeild of length args(0) or less
 *  Reads from args(1) and writes to args(2)
 */

object GetSmallbank {
	def main(args : Array[String]) : Unit = {
		val maxlen = args(0).toInt
		val pcfg = new PCFG
		val data = TreebankData.read(args(1),pcfg)
		val smallbank = data.filter(_.terminals.length <= maxlen)
		println("Got " + smallbank.size + " trees of length " + maxlen  + " or less")
	    TreebankData.write(args(2),smallbank,pcfg)
		-1
	} 
}
