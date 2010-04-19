package tools

import parse._
import corpora.treebank._
import java.io.{BufferedWriter,FileWriter}

object UnkText {

	def main(args : Array[String]) : Unit = {
	  val pcfg = new PCFG
	  val data = TreebankData.read(args(0),pcfg) //read in an unked treebank
	  val unker = new UnkLeastCommon(5,data,pcfg)
	  val bw = new BufferedWriter(new FileWriter(args(2))) 
   
	  def printList(ls : List[String]) = {
	    if(ls.length > 0) {
	    	bw.write(ls(0))
	    	ls.drop(1).foreach(w => bw.write(" " + w))
	    }
	    bw.write("\n")
	  }
   
	  val lines = io.Source.fromFile(args(1)).getLines
	  lines.map(_.replaceAll("\\n","").split("\\s")).map(ls => { 
		  unker.unkSentence(ls.toList)
	  }).foreach(printList(_))
   
	  bw.close
	}
}
