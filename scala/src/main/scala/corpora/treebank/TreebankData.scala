package corpora.treebank

import java.io.{File,FileWriter,BufferedWriter}
import parse._
import io.Source
import java.io.StringReader
import java.lang.StringBuffer


object TreebankData {
  
	def read(filename : String, pcfg : PCFG) : List[ParseTree] = {
	  print("Reading " + filename + " for treebank format trees ... ")
	  val filedata = Source.fromFile(filename).getLines
	  val treestrs = (List[String]() /: filedata)((a,b) => if(b.charAt(0) != '(') (a(0) + b) :: a.drop(1) else b :: a)
	  println("Got " + treestrs.length + " trees")
	  treestrs.reverse.map(s => pcfg.growTree(s))
	} 
 
	def write(filename : String, data : List[ParseTree], pcfg : PCFG) = {
		var bw = new BufferedWriter(new FileWriter(new File(filename)))
		data.foreach(d => bw.write("\\s+".r.replaceAllIn(PCFGPrinter.treeToString(pcfg,d)," ") + "\n"))
		bw.close
	}
}






